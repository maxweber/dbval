(ns ^:no-doc dbval.db
  (:require
    #?(:cljs [goog.array :as garray])
    [clojure.walk]
    [clojure.data]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clojure.java.io :as io]
    #?(:clj [dbval.inline :refer [update]])
    [dbval.lru :as lru]
    [dbval.util :as util]
    [me.tonsky.persistent-sorted-set :as set]
    [me.tonsky.persistent-sorted-set.arrays :as arrays]
    [taoensso.nippy :as nippy]
    [com.yetanalytics.squuid :as squuid])
  #?(:clj (:import clojure.lang.IFn$OOL))
  #?(:cljs (:require-macros [dbval.db :refer [case-tree combine-cmp defn+ defcomp defrecord-updatable int-compare validate-attr validate-val]]))
  (:refer-clojure :exclude [seqable? #?(:clj update)]))

#?(:clj (set! *warn-on-reflection* true))

(declare transact-tx-data)

;; ----------------------------------------------------------------------------

#?(:cljs
   (do
     (def Exception js/Error)
     (def IllegalArgumentException js/Error)
     (def UnsupportedOperationException js/Error)))

;; tx0 is the nil UUID, used as a minimum bound for transaction ID comparisons
;; Squuids are time-ordered, so this will always be less than any real tx ID
(def tx0
  #uuid "00000000-0000-0000-0000-000000000000")

(defn uuid<=
  "Compares two UUIDs. Returns true if a <= b."
  [a b]
  (<= (compare a b) 0))

(def ^:const implicit-schema
  {:db/ident {:db/unique :db.unique/identity}})

(declare tuple?)

;; ----------------------------------------------------------------------------

(defn #?@(:clj  [^Boolean seqable?]  
          :cljs [^boolean seqable?])
  [x]
  (and (not (string? x))
    #?(:cljs (or (cljs.core/seqable? x)
               (arrays/array? x))
       :clj  (or (seq? x)
               (instance? clojure.lang.Seqable x)
               (nil? x)
               (instance? Iterable x)
               (arrays/array? x)
               (instance? java.util.Map x)))))

;; ----------------------------------------------------------------------------
;; macros and funcs to support writing defrecords and updating
;; (replacing) builtins, i.e., Object/hashCode, IHashEq hasheq, etc.
;; code taken from prismatic:
;;  https://github.com/Prismatic/schema/commit/e31c419c56555c83ef9ee834801e13ef3c112597
;;

(defn- cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

#?(:clj
   (defmacro if-cljs
     "Return then if we are generating cljs code and else for Clojure code.
     https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
     [then else]
     (if (cljs-env? &env) then else)))

#?(:clj
   (defn patch-tag [meta cljs-env?]
     (if cljs-env?
       meta
       (condp = (:tag meta)
         'boolean (assoc meta :tag java.lang.Boolean)
         'number  (assoc meta :tag clojure.core$long)
         meta))))

#?(:clj
   (defmacro defn+
     "CLJS really don't like :declared metadata on vars (generates less
      efficient code), but it needs it to skip warnings. So we redefine
      first with ^:declared and empty implementation, and then immediately
      redefine again without ^:declared. This way both `declare` and `defn+`
      versions have no ^:declared meta, thus allowing CLJS to generate direct
      invocations and see type hints."
     [name & rest]
     (let [name'    (vary-meta name patch-tag (cljs-env? &env))
           arglists (if (vector? (first rest))
                      [(first rest)]
                      (map #(list (first %)) rest))]
       (if (cljs-env? &env)
         `(do
            (defn ~(vary-meta name' assoc :declared true) ~@arglists)
            (defn ~name' ~@rest))
         `(defn ~name' ~@rest)))))

(defn combine-hashes [x y]
  #?(:clj  (clojure.lang.Util/hashCombine x y)
     :cljs (hash-combine x y)))

#?(:clj
   (defn- get-sig [method]
     ;; expects something like '(method-symbol [arg arg arg] ...)
     ;; if the thing matches, returns [fully-qualified-symbol arity], otherwise nil
     (and (sequential? method)
       (symbol? (first method))
       (vector? (second method))
       (let [sym (first method)
             ns  (or (some->> sym resolve meta :ns str) "clojure.core")]
         [(symbol ns (name sym)) (-> method second count)]))))

#?(:clj
   (defn- dedupe-interfaces [deftype-form]
     ;; get the interfaces list, remove any duplicates, similar to remove-nil-implements in potemkin
     ;; verified w/ deftype impl in compiler:
     ;; (deftype* tagname classname [fields] :implements [interfaces] :tag tagname methods*)
     (let [[deftype* tagname classname fields implements interfaces & rest] deftype-form]
       (when (or (not= deftype* 'deftype*) (not= implements :implements))
         (throw (IllegalArgumentException. "deftype-form mismatch")))
       (list* deftype* tagname classname fields implements (vec (distinct interfaces)) rest))))

#?(:clj
   (defn- make-record-updatable-clj [name fields & impls]
     (let [impl-map (->> impls (map (juxt get-sig identity)) (filter first) (into {}))
           body     (macroexpand-1 (list* 'defrecord name fields impls))]
       (clojure.walk/postwalk
         (fn [form]
           (if (and (sequential? form) (= 'deftype* (first form)))
             (->> form
               dedupe-interfaces
               (remove (fn [method]
                         (when-some [impl (-> method get-sig impl-map)]
                           (not= method impl)))))
             form))
         body))))

#?(:clj
   (defn- make-record-updatable-cljs [name fields & impls]
     `(do
        (defrecord ~name ~fields)
        (extend-type ~name ~@impls))))

#?(:clj
   (defmacro defrecord-updatable [name fields & impls]
     `(if-cljs
        ~(apply make-record-updatable-cljs name fields impls)
        ~(apply make-record-updatable-clj  name fields impls))))

;; ----------------------------------------------------------------------------

#?(:clj  (declare hash-datom)
   :cljs (defn ^number hash-datom [d]))

#?(:clj  (declare equiv-datom)
   :cljs (defn ^boolean equiv-datom [d o]))

#?(:clj  (declare seq-datom)
   :cljs (defn seq-datom [d]))

#?(:clj  (declare nth-datom)
   :cljs (defn nth-datom ([d i]) ([d i not-found])))

#?(:clj  (declare assoc-datom)
   :cljs (defn assoc-datom [d k v]))

#?(:clj  (declare val-at-datom)
   :cljs (defn val-at-datom [d k not-found]))

(defprotocol IDatom
  (datom-tx [this])
  (datom-added [this])
  (datom-get-idx [this])
  (datom-set-idx [this value]))

(deftype Datom #?(:clj [e a v tx added ^:unsynchronized-mutable ^int idx ^:unsynchronized-mutable ^int _hash]
                  :cljs [e a v tx added ^:mutable ^number idx ^:mutable ^number _hash])
  IDatom
  (datom-tx [d] tx)
  (datom-added [d] added)
  (datom-get-idx [_] idx)
  (datom-set-idx [_ value] (set! idx (int value)))

  #?@(:cljs
      [IHash
       (-hash [d] (if (zero? _hash)
                    (set! _hash (hash-datom d))
                    _hash))
       IEquiv
       (-equiv [d o] (and (instance? Datom o) (equiv-datom d o)))

       ISeqable
       (-seq [d] (seq-datom d))

       ILookup
       (-lookup [d k] (val-at-datom d k nil))
       (-lookup [d k nf] (val-at-datom d k nf))

       IIndexed
       (-nth [this i] (nth-datom this i))
       (-nth [this i not-found] (nth-datom this i not-found))
        
       IAssociative
       (-assoc [d k v] (assoc-datom d k v))

       IPrintWithWriter
       (-pr-writer [d writer opts]
         (pr-sequential-writer writer pr-writer
           "#dbval/Datom [" " " "]"
           opts [(.-e d) (.-a d) (.-v d) (datom-tx d) (datom-added d)]))]
      :clj
      [Object
       (hashCode [d]
         (if (zero? _hash)
           (let [h (int (hash-datom d))]
             (set! _hash h)
             h)
           _hash))
       (toString [d] (pr-str d))

       clojure.lang.IHashEq
       (hasheq [d] (.hashCode d))

       clojure.lang.Seqable
       (seq [d] (seq-datom d))

       clojure.lang.IPersistentCollection
       (equiv [d o] (and (instance? Datom o) (equiv-datom d o)))
       (empty [d] (throw (UnsupportedOperationException. "empty is not supported on Datom")))
       (count [d] 5)
       (cons [d [k v]] (assoc-datom d k v))
        
       clojure.lang.Indexed
       (nth [this i]           (nth-datom this i))
       (nth [this i not-found] (nth-datom this i not-found))

       clojure.lang.ILookup
       (valAt [d k] (val-at-datom d k nil))
       (valAt [d k nf] (val-at-datom d k nf))

       clojure.lang.Associative
       (entryAt [d k] (some->> (val-at-datom d k nil) (clojure.lang.MapEntry k)))
       (containsKey [e k] (#{:e :a :v :tx :added} k))
       (assoc [d k v] (assoc-datom d k v))]))

#?(:cljs (goog/exportSymbol "dbval.db.Datom" Datom))

(defn ^Datom datom
  ([e a v] (Datom. e a v tx0 true 0 0))
  ([e a v tx] (Datom. e a v tx true 0 0))
  ([e a v tx added] (Datom. e a v tx (boolean added) 0 0)))

(defn datom? [x] (instance? Datom x))

(defn+ ^:private hash-datom [^Datom d]
  (-> (hash (.-e d))
    (combine-hashes (hash (.-a d)))
    (combine-hashes (hash (.-v d)))))

(defn+ ^:private equiv-datom [^Datom d ^Datom o]
  (and (= (.-e d) (.-e o))
    (= (.-a d) (.-a o))
    (= (.-v d) (.-v o))))

(defn+ ^:private seq-datom [^Datom d]
  (list (.-e d) (.-a d) (.-v d) (datom-tx d) (datom-added d)))

;; keep it fast by duplicating for both keyword and string cases
;; instead of using sets or some other matching func
(defn+ ^:private val-at-datom [^Datom d k not-found]
  (cond
    (keyword? k)
    (case k
      :e     (.-e d)
      :a     (.-a d)
      :v     (.-v d)
      :tx    (datom-tx d)
      :added (datom-added d)
      not-found)
    
    (string? k)
    (case k
      "e"     (.-e d)
      "a"     (.-a d)
      "v"     (.-v d)
      "tx"    (datom-tx d)
      "added" (datom-added d)
      not-found)
    
    :else
    not-found))

(defn+ ^:private nth-datom
  ([^Datom d ^long i]
   (case i
     0 (.-e d)
     1 (.-a d)
     2 (.-v d)
     3 (datom-tx d)
     4 (datom-added d)
     #?(:clj  (throw (IndexOutOfBoundsException.))
        :cljs (throw (js/Error. (str "Datom/-nth: Index out of bounds: " i))))))
  ([^Datom d ^long i not-found]
   (case i
     0 (.-e d)
     1 (.-a d)
     2 (.-v d)
     3 (datom-tx d)
     4 (datom-added d)
     not-found)))

(defn+ ^:private ^Datom assoc-datom [^Datom d k v]
  (case k
    :e     (datom v       (.-a d) (.-v d) (datom-tx d) (datom-added d))
    :a     (datom (.-e d) v       (.-v d) (datom-tx d) (datom-added d))
    :v     (datom (.-e d) (.-a d) v       (datom-tx d) (datom-added d))
    :tx    (datom (.-e d) (.-a d) (.-v d) v            (datom-added d))
    :added (datom (.-e d) (.-a d) (.-v d) (datom-tx d) v)
    (throw (IllegalArgumentException. (str "invalid key for #dbval/Datom: " k)))))

;; printing and reading
;; #datomic/DB {:schema <map>, :datoms <vector of [e a v tx]>}

(defn ^Datom datom-from-reader [vec]
  (apply datom vec))

#?(:clj
   (defmethod print-method Datom [^Datom d, ^java.io.Writer w]
     (.write w (str "#dbval/Datom "))
     (binding [*out* w]
       (pr [(.-e d) (.-a d) (.-v d) (datom-tx d) (datom-added d)]))))

;; ----------------------------------------------------------------------------
;; datom cmp macros/funcs
;;

#?(:clj
   (defmacro combine-cmp [& comps]
     (loop [comps (reverse comps)
            res   (num 0)]
       (if (not-empty comps)
         (recur
           (next comps)
           `(let [c# ~(first comps)]
              (if (== 0 c#)
                ~res
                c#)))
         res))))

#?(:clj
   (defn- -case-tree [queries variants]
     (if queries
       (let [v1 (take (/ (count variants) 2) variants)
             v2 (drop (/ (count variants) 2) variants)]
         (list 'if (first queries)
           (-case-tree (next queries) v1)
           (-case-tree (next queries) v2)))
       (first variants))))

#?(:clj
   (defmacro case-tree [qs vs]
     (-case-tree qs vs)))

(defn cmp
  #?(:clj
     {:inline
      (fn [x y]
        `(let [x# ~x y# ~y]
           (if (nil? x#) 0 (if (nil? y#) 0 (long (compare x# y#))))))})
  ^long [x y]
  (if (nil? x) 0 (if (nil? y) 0 (long (compare x y)))))

(defn class-identical?
  #?(:clj  {:inline (fn [x y] `(identical? (class ~x) (class ~y)))})
  [x y]
  #?(:clj  (identical? (class x) (class y))
     :cljs (identical? (type x) (type y))))

#?(:clj
   (defn class-name
     {:inline
      (fn [x]
        `(let [^Object x# ~x]
           (if (nil? x#) x# (.getName (. x# (getClass))))))}
     ^String [^Object x] (if (nil? x) x (.getName (. x (getClass))))))

(defn class-compare
  ^long [x y]
  #?(:clj  (long (compare (class-name x) (class-name y)))
     :cljs (garray/defaultCompare (type->str (type x)) (type->str (type y)))))

#?(:clj
   (defmacro int-compare [x y]
     `(if-cljs
        (- ~x ~y)
        (long (Integer/compare ~x ~y)))))

(defn ihash
  {:inline (fn [x] `(. clojure.lang.Util (hasheq ~x)))}
  ^long [x]
  #?(:clj  (. clojure.lang.Util (hasheq x))
     :cljs (hash x)))

#?(:clj  (declare value-compare)
   :cljs (defn ^number value-compare [x y]))

(defn- seq-compare [xs ys]
  (let [cx (count xs)
        cy (count ys)]
    (cond
      (< cx cy)
      -1
      
      (> cx cy)
      1
      
      :else
      (loop [xs xs
             ys ys]
        (if (empty? xs)
          0
          (let [x (first xs)
                y (first ys)]
            (cond
              (and (nil? x) (nil? y))
              (recur (next xs) (next ys))
                
              (nil? x)
              -1
                
              (nil? y)
              1
                
              :else
              (let [v (value-compare x y)]
                (if (= v 0)
                  (recur (next xs) (next ys))
                  v)))))))))

(defn+ ^number value-compare [x y]
  (try
    (cond
      (= x y) 0
      (and (sequential? x) (sequential? y)) (seq-compare x y)
      #?@(:clj  [(instance? Number x)       (clojure.lang.Numbers/compare x y)])
      #?@(:clj  [(instance? Comparable x)   (.compareTo ^Comparable x y)]
          :cljs [(satisfies? IComparable x) (-compare x y)])
      (not (class-identical? x y)) (class-compare x y)
      #?@(:cljs [(or (number? x) (string? x) (array? x) (true? x) (false? x)) (garray/defaultCompare x y)])
      :else (int-compare (ihash x) (ihash y)))
    (catch #?(:clj ClassCastException :cljs js/Error) e
      (if (not (class-identical? x y))
        (class-compare x y)
        (throw e)))))

(defn value-cmp
  #?(:clj
     {:inline
      (fn [x y]
        `(let [x# ~x y# ~y]
           (if (nil? x#) 0 (if (nil? y#) 0 (value-compare x# y#)))))})
  ^long [x y]
  (if (nil? x)
    0
    (if (nil? y)
      0
      (value-compare x y))))

;; Slower cmp-* fns allows for datom fields to be nil.
;; Such datoms come from slice method where they are used as boundary markers.

#?(:clj
   (defmacro defcomp [sym [arg1 arg2] & body]
     (let [a1 (with-meta arg1 {})
           a2 (with-meta arg2 {})]
       `(if-cljs
          (defn ~sym [~arg1 ~arg2]
            ~@body)
          (def ~sym
            (reify
              java.util.Comparator
              (compare [_# ~a1 ~a2]
                (let [~arg1 ~arg1 ~arg2 ~arg2]
                  ~@body))
              clojure.lang.IFn
              (invoke [this# ~a1 ~a2]
                (.compare this# ~a1 ~a2))
              IFn$OOL
              (invokePrim [this# ~a1 ~a2]
                (.compare this# ~a1 ~a2))))))))

(defcomp cmp-datoms-eavt ^long [^Datom d1, ^Datom d2]
  (combine-cmp
    (int-compare (.-e d1) (.-e d2))
    (cmp (.-a d1) (.-a d2))
    (value-cmp (.-v d1) (.-v d2))
    (int-compare (datom-tx d1) (datom-tx d2))))

(defcomp cmp-datoms-aevt ^long [^Datom d1, ^Datom d2]
  (combine-cmp
    (cmp (.-a d1) (.-a d2))
    (int-compare (.-e d1) (.-e d2))
    (value-cmp (.-v d1) (.-v d2))
    (int-compare (datom-tx d1) (datom-tx d2))))

(defcomp cmp-datoms-avet ^long [^Datom d1, ^Datom d2]
  (combine-cmp
    (cmp (.-a d1) (.-a d2))
    (value-cmp (.-v d1) (.-v d2))
    (int-compare (.-e d1) (.-e d2))
    (int-compare (datom-tx d1) (datom-tx d2))))

;; fast versions without nil checks

(defn- cmp-attr-quick
  #?(:clj
     {:inline
      (fn [a1 a2]
        `(long (.compareTo ~(with-meta a1 {:tag "Comparable"}) ~a2)))})
  ^long [a1 a2]
  ;; either both are keywords or both are strings
  #?(:cljs
     (if (keyword? a1)
       (-compare a1 a2)
       (garray/defaultCompare a1 a2))
     :clj
     (.compareTo ^Comparable a1 a2)))

(defcomp cmp-datoms-eav-quick ^long [^Datom d1, ^Datom d2]
  (combine-cmp
    (int-compare (.-e d1) (.-e d2))
    (cmp-attr-quick (.-a d1) (.-a d2))
    (value-compare (.-v d1) (.-v d2))))

(defcomp cmp-datoms-eavt-quick ^long [^Datom d1, ^Datom d2]
  (combine-cmp
    (int-compare (.-e d1) (.-e d2))
    (cmp-attr-quick (.-a d1) (.-a d2))
    (value-compare (.-v d1) (.-v d2))
    (int-compare (datom-tx d1) (datom-tx d2))))

(defcomp cmp-datoms-aevt-quick ^long [^Datom d1, ^Datom d2]
  (combine-cmp
    (cmp-attr-quick (.-a d1) (.-a d2))
    (int-compare (.-e d1) (.-e d2))
    (value-compare (.-v d1) (.-v d2))
    (int-compare (datom-tx d1) (datom-tx d2))))

(defcomp cmp-datoms-avet-quick ^long [^Datom d1, ^Datom d2]
  (combine-cmp
    (cmp-attr-quick (.-a d1) (.-a d2))
    (value-compare (.-v d1) (.-v d2))
    (int-compare (.-e d1) (.-e d2))
    (int-compare (datom-tx d1) (datom-tx d2))))

(defn- diff-sorted [a b cmp]
  (loop [only-a []
         only-b []
         both   []
         a      a
         b      b]
    (cond
      (empty? a) [(not-empty only-a) (not-empty (into only-b b)) (not-empty both)]
      (empty? b) [(not-empty (into only-a a)) (not-empty only-b) (not-empty both)]
      :else
      (let [first-a (first a)
            first-b (first b)
            diff (try
                   (cmp first-a first-b)
                   (catch #?(:clj ClassCastException :cljs js/Error) _
                     :incomparable))]
        (cond
          (= diff :incomparable) (recur (conj only-a first-a) (conj only-b first-b) both                (next a) (next b))
          (== diff 0)            (recur only-a                only-b                (conj both first-a) (next a) (next b))
          (< diff 0)             (recur (conj only-a first-a) only-b                both                (next a) b)
          (> diff 0)             (recur only-a                (conj only-b first-b) both                a        (next b)))))))

;; ----------------------------------------------------------------------------

#?(:clj  (declare hash-db)
   :cljs (defn ^number hash-db [db]))

#?(:clj  (declare hash-fdb)
   :cljs (defn ^number hash-fdb [db]))

#?(:clj  (declare equiv-db)
   :cljs (defn ^boolean equiv-db [db other]))

#?(:clj  (declare restore-db)
   :cljs (defn restore-db [keys]))

#?(:clj  (declare indexing?)
   :cljs (defn ^boolean indexing? [db attr]))

#?(:cljs (defn pr-db [db w opts]))

#?(:clj  (declare resolve-datom)
   :cljs (defn resolve-datom [db e a v t default-e default-tx]))

#?(:clj  (declare components->pattern)
   :cljs (defn components->pattern [db index c0 c1 c2 c3 default-e default-tx]))

#?(:clj  (declare resolve-datom*)
   :cljs (defn resolve-datom* [db e a v t default-e default-tx]))

#?(:clj  (declare components->pattern*)
   :cljs (defn components->pattern* [db index c0 c1 c2 c3 default-e default-tx]))

;;;;;;;;;; Fast validation

#?(:clj
   (defmacro validate-attr [attr at]
     `(let [attr# ~attr]
        (when-not (or
                    (keyword? attr#)
                    (string? attr#))
          (let [at# ~at]
            (util/raise "Bad entity attribute " attr# " at " at# ", expected keyword or string"
              {:error :transact/syntax, :attribute attr#, :context at#}))))))

#?(:clj
   (defmacro validate-val [v at]
     `(when (nil? ~v)
        (let [at# ~at]
          (util/raise "Cannot store nil as a value at " at#
            {:error :transact/syntax, :value nil, :context at#})))))

;;;;;;;;;; Searching

(defprotocol ISearch
  (-search [data pattern]))

(defn- ^Datom fsearch [data pattern]
  (first (-search data pattern)))

(defprotocol IIndexAccess
  (-datoms [db index c0 c1 c2 c3])
  (-seek-datoms [db index c0 c1 c2 c3])
  (-rseek-datoms [db index c0 c1 c2 c3])
  (-index-range [db attr start end]))

(defn validate-indexed [db index c0 c1 c2 c3]
  (when (= index :avet)
    (when-some [attr c0]
      (when-not (indexing? db attr)
        (util/raise "Attribute " attr " should be marked as :db/index true"
          {:error :index-access :index :avet :components [c0 c1 c2 c3]})))))

(defprotocol IDB
  (-schema [db])
  (-attrs-by [db property]))

;; ----------------------------------------------------------------------------

(defn db-transient [db]
  (-> db
    (update :eavt transient)
    (update :aevt transient)
    (update :avet transient)))

(defn db-persistent! [db]
  (-> db
    (update :eavt persistent!)
    (update :aevt persistent!)
    (update :avet persistent!)))

#?(:clj
   (defn vpred [v]
     (cond
       (string? v)  (fn [x] (if (string? x) (.equals ^String v x) false))
       (int? v)     (fn [x] (if (int? x) (= (long v) (long x)) false))
       (keyword? v) (fn [x] (.equals ^Object v x))
       (nil? v)     (fn [x] (nil? x))
       :else        (fn [x] (= v x)))))

(defn ^com.apple.foundationdb.tuple.Tuple tuple
  "Turns the `components` into a `com.apple.foundationdb.tuple.Tuple`."
  [& components]
  (.addAll (com.apple.foundationdb.tuple.Tuple.)
           ^java.util.List
           components))

(defn serialize-tuple
  [x]
  (cond
    (or (keyword? x)
        (symbol? x)
        (string? x)
        #?(:clj (instance? java.util.Date x)
           :cljs false))
    (pr-str x)

    (sequential? x)
    (apply tuple
           (map serialize-tuple
                x))
    :else
    x))

(defn serialize-value
  [db attr v]
  (cond
    (or (map? v)
        (keyword? v)
        (symbol? v)
        (string? v)
        #?(:clj (instance? java.util.Date v)
           :cljs false))
    (pr-str v)

    (sequential? v)
    (serialize-tuple v)

    :else
    v))

(defn tuple-list
  [db order datom]
  (try
    (let [[e a v t added] datom]
      (case (keyword order)
        :eavt
        (list (name order) e (when a (pr-str a)) (serialize-value db a v) t added)
        :aevt
        (list (name order) (when a (pr-str a)) e (serialize-value db a v) t added)
        :avet
        (list (name order) (when a (pr-str a)) (serialize-value db a v) e t added)
        :teav
        (list (name order) t e (when a (pr-str a)) (serialize-value db a v) added)
        ))
    (catch Exception e
      (throw (ex-info "tuple-list failed"
                      {:order order
                       :datom datom}
                      e)))))

(defn tuple-range
  "Turns the `components` into a `com.apple.foundationdb.tuple.Tuple` and returns
   a vector of the begin and end of the tuple's range."
  [& components]
  (let [r (.range ^com.apple.foundationdb.tuple.Tuple
                  (apply tuple
                         components))]
    [(.begin r)
     (.end r)]))

(defn ^com.apple.foundationdb.tuple.Tuple datom-tuple
  "Converts a datom to a `com.apple.foundationdb.tuple.Tuple` and sorts the
   components according to the `order`."
  ([db order datom]
   (apply tuple
          (tuple-list db
                      order
                      datom)))
  ([db datom]
   (datom-tuple db
                :eavt
                datom)))

(defn deserialize-tuple
  [x]
  (cond
    (string? x)
    (edn/read-string x)

    (instance? java.util.List
               x)
    (into []
          (map deserialize-tuple)
          x)
    :else
    x))

(defn deserialize-value
  [db attr v]
  (if (string? v)
    (edn/read-string v)
    (if (tuple? db attr)
      (deserialize-tuple v)
      (if (get-in db [:schema attr :db/tupleAttrs])
        (deserialize-tuple v)
        v))))

(defn datom-from-tuple
  "Reads back a datom that was stored as `com.apple.foundationdb.tuple.Tuple`."
  [db tuple]
  (try
    (let [[order c0 c1 c2 c3 c4] (vec tuple)]
      (case order
        "eavt"
        (let [attr (edn/read-string c1)]
          (datom c0 attr (deserialize-value db attr c2) c3 c4))
        "aevt"
        (let [attr (edn/read-string c0)]
          (datom c1 attr (deserialize-value db attr c2) c3 c4))
        "avet"
        (let [attr (edn/read-string c0)]
          (datom c2 attr (deserialize-value db attr c1) c3 c4))
        "teav"
        (let [attr (edn/read-string c2)]
          (datom c1 attr (deserialize-value db attr c3) c0 c4))))
    (catch Exception e
      (throw (ex-info "datom-from-tuple failed"
                      {:tuple tuple}
                      e)))))

(defn tuple-from-bytes
  "Converts a byte array into a `com.apple.foundationdb.tuple.Tuple`."
  [^bytes bytes]
  (com.apple.foundationdb.tuple.Tuple/fromBytes bytes))

(defn bytes-to-datoms-xf
  [db]
  (comp
   (partial datom-from-tuple
            db)
   tuple-from-bytes))

(defn bytes-to-datoms
  "Converts a collection of byte array (`com.apple.foundationdb.tuple.Tuple`) into
   datoms."
  [db byte-tuples]
  (->Eduction
   (map (bytes-to-datoms-xf db))
   byte-tuples))

(defn byte-array-compare
  [^bytes a ^bytes b]
  (java.util.Arrays/compareUnsigned
   a
   b))

(def byte-array-comparator
  (reify java.util.Comparator
    (compare [_ a b]
      (byte-array-compare ^bytes a ^bytes b))))

(defn pack
  [^com.apple.foundationdb.tuple.Tuple tuple]
  (try
    (.pack tuple)
    (catch Exception e
      (throw (ex-info "pack failed"
                      {:tuple tuple}
                      e)))))

(defn case-pick
  [case datom]
  (into []
        (map (fn [component]
               (get datom
                    (keyword component))))
        case))

(declare slice)

(defn datoms-filter
  "Will remove all retracted `datoms`.

   No matter which index is used (`:eavt`, `:aevt`, `:avet` or `:vaet`) the last
   two components are always the transaction id and a boolean flag that
   indicates if the datom is a `:db/add` or `:db/retract`. These two components
   are also sorted, due to the transaction id everything is in the order in which
   the tx-ops where transacted. If a datom is retracted in the current database
   value, then the `:db/add` will be directly followed by a corresponding
   `:db/retract` datom, and the logic here will remove both from the returned
   sequence of `datoms`. All `:db/retract` datoms are removed in any case."
  [rf]
  (let [previous (volatile! nil)]
    (fn
      ([] (rf))
      ([result]
       (let [d1 @previous]
         (if (:added d1)
           (rf (rf result
                   d1))
           (rf result))))
      ([result d2]
       (if-not @previous
         (do
           (vreset! previous d2)
           result)
         (let [d1 @previous]
           (let [eav= (and (= (:e d1) (:e d2))
                           (= (:a d1) (:a d2))
                           (= (:v d1) (:v d2)))]
             (cond
               (and eav=
                    (:added d1)
                    (not (:added d2)))
               (do
                 ;; (prn "later tx retract" d1 d2)
                 (vreset! previous nil) ;; next step should ignore d2
                 result) ;; do not add d1 since it was retracted by d2 in a later transaction

               (and eav=
                    (= (:tx d1)
                       (:tx d2))
                    (not (:added d1))
                    (:added d2))
               (do
                 ;; (prn "same tx retract" d1 d2)
                 (vreset! previous nil) ;; next step should ignore d2
                 result) ;; add nothing since datom was retracted in the same transaction.

               (not (:added d2))
               (do
                 ;; (prn "d2 retract" d1 d2)
                 (vreset! previous d2)
                 result)

               :else
               (do
                 ;; (prn "else" d1 d2)
                 (vreset! previous
                          d2)
                 (rf result d1))
               ))))))))

(defn sort-components
  [order [c0 c1 c2 c3]]
  (case order
    :eavt [c0 c1 c2 c3]
    :aevt [c1 c0 c2 c3]
    :avet [c2 c0 c1 c3]
    :teav [c3 c0 c1 c2]
    ))

(defn datom=
  [[e a v tx] datom]
  (and (or (not e)
           (= e (:e datom)))
       (or (not a)
           (= a (:a datom)))
       (or (not (some? v))
           (= v (:v datom)))
       (or (not tx)
           (= tx (:tx datom)))))

(defn pattern->order
  [db pattern]
  (let [[e a v tx] pattern]
    (if e
      :eavt
      (if a
        (if (indexing? db
                       a)
          :avet
          :aevt)
        :eavt))))

(defn- sqlite-jdbc-url
  "Builds a full SQLite JDBC URL with optional pragmas."
  [db-file {:keys [busy-timeout-ms foreign-keys? wal? read-only? mmap-size sync]
            :or   {busy-timeout-ms 5000
                   foreign-keys?   true
                   wal?            true
                   read-only?      false
                   sync            "NORMAL"}}]
  (let [params (cond-> {"busy_timeout" busy-timeout-ms
                        "foreign_keys" (if foreign-keys? "on" "off")
                        "synchronous"  (str sync)}
                 wal?       (assoc "journal_mode" "WAL")
                 read-only? (assoc "mode" "ro")
                 mmap-size  (assoc "mmap_size" mmap-size))
        qs     (->> params
                    (map (fn [[k v]] (str k "=" v)))
                    (str/join "&"))]
    (str "jdbc:sqlite:" db-file (when (seq qs) (str "?" qs)))))

(defn ^java.sql.Connection get-sqlite-connection
  "Returns a java.sql.Connection for the given SQLite file.
   Caller must close the connection manually."
  [{:keys [db-file opts]}]
  ;; explicitly load the driver class
  (java.lang.Class/forName "org.sqlite.JDBC")
  (let [^String url (sqlite-jdbc-url db-file opts)
        conn (java.sql.DriverManager/getConnection url)]
    (.setAutoCommit conn false)
    conn))

(defn slice
  [{:keys [db ^bytes begin ^bytes end reverse]}]
  (reify java.lang.Iterable
    (iterator [_]
      (let [^java.sql.Connection conn (:conn db)
            ^java.sql.PreparedStatement stmt
            (.prepareStatement conn
                               (str "select k from dbval where k >= ? and k < ?"
                                    (when reverse
                                      " order by k desc"))
                               java.sql.ResultSet/TYPE_FORWARD_ONLY
                               java.sql.ResultSet/CONCUR_READ_ONLY)
            _ (.setBytes stmt 1 begin)
            _ (.setBytes stmt 2 end)
            _ (.setFetchSize ^java.sql.Statement stmt 1000) ; hint (SQLite may ignore)
            ^java.sql.ResultSet rs (.executeQuery stmt)

            next-val (atom nil)
            advanced (atom false)
            closed   (atom false)
            close!   (fn []
                       (when-not @closed
                         (reset! closed true)
                         (try (.close rs)   (catch Throwable _))
                         (try (.close stmt) (catch Throwable _))
                         ))
            advance! (fn []
                       (when-not @closed
                         (if (.next rs)
                           (do (reset! next-val (.getBytes rs "k")) true)
                           (do (reset! next-val nil) (close!) false))))]

        (reify java.util.Iterator
          (hasNext [this]
            (or @advanced
                (reset! advanced
                        (boolean (advance!)))))
          (next [_]
            (let [ok (or @advanced (advance!))]
              (when-not ok
                (throw (java.util.NoSuchElementException.)))
              (let [v @next-val]
                (reset! advanced false)
                (reset! next-val nil)
                v)))
          (remove [_]
            (throw (UnsupportedOperationException. "remove not supported"))))))))

(defrecord-updatable DB [schema max-tx rschema pull-patterns pull-attrs hash
                         #?@(:clj [db-file conn])]
  #?@(:cljs
      [IHash                (-hash  [db]        (hash-db db))
       IEquiv               (-equiv [db other]  (equiv-db db other))
       IReversible          (-rseq  [db]        (-rseq (.-eavt db)))
       ICounted             (-count [db]        (count (.-eavt db)))
       IEmptyableCollection (-empty [db]        (-> (restore-db
                                                      {:schema  (.-schema db)
                                                       :rschema (.-rschema db)
                                                       :eavt    (empty (.-eavt db))
                                                       :aevt    (empty (.-aevt db))
                                                       :avet    (empty (.-avet db))})
                                                  (with-meta (meta db))))
       IPrintWithWriter     (-pr-writer [db w opts] (pr-db db w opts))
       IEditableCollection  (-as-transient [db] (db-transient db))
       ITransientCollection (-conj! [db key] (throw (ex-info "dbval.DB/conj! is not supported" {})))
       (-persistent! [db] (db-persistent! db))]

      :clj
      [Object               (hashCode [db]      (hash-db db))
       clojure.lang.IHashEq (hasheq [db]        (hash-db db))
       clojure.lang.IEditableCollection
       (asTransient [db] (db-transient db))
       clojure.lang.ITransientCollection
       (conj [db key] (throw (ex-info "dbval.DB/conj! is not supported" {})))
       (persistent [db] (db-persistent! db))])

  IDB
  (-schema [db] (.-schema db))
  (-attrs-by [db property] ((.-rschema db) property))

  ISearch
  (-search [db pattern]
    (let [[e a v tx] pattern
          pred       #?(:clj  (vpred v)
                        :cljs #(= v %))
          multival?  (contains? (-attrs-by db :db.cardinality/many) a)
          index (pattern->order db
                                pattern)
          [begin end] (apply tuple-range
                             (name index)
                             (take-while
                              some?
                              (rest
                               (tuple-list db
                                           index
                                           [e
                                            a
                                            v
                                            tx]))))
          ]
      (->Eduction
       (comp (map (bytes-to-datoms-xf db))
             (filter (fn [datom]
                       (uuid<= (:tx datom)
                               (:max-tx db))))
             datoms-filter
             (filter (partial datom=
                              [e a v tx])))
       (slice {:db db
               :begin begin
               :end end}))))

  IIndexAccess
  (-datoms [db index c0 c1 c2 c3]
    (validate-indexed db index c0 c1 c2 c3)
    (let [[e a v tx] (sort-components
                      index
                      [c0 c1 c2 c3])
          datom-coll (resolve-datom* db e a v tx)
          [e a v tx] datom-coll
          components         (take-while
                              some?
                              (rest
                               (tuple-list db
                                           index
                                           [e
                                            a
                                            v
                                            tx])))
          [begin end] (apply tuple-range
                             (name index)
                             components)]
      (->Eduction
       (comp (map (bytes-to-datoms-xf db))
               (filter (fn [datom]
                         (uuid<= (:tx datom)
                                 (:max-tx db))))
               datoms-filter
               (filter (partial datom=
                                [e a v tx])))
       (slice {:db db
               :begin begin
               :end end}))))

  (-seek-datoms [db index c0 c1 c2 c3]
    (validate-indexed db index c0 c1 c2 c3)
    (let [[e a v tx] (sort-components
                      index
                      [c0 c1 c2 c3])
          [e a v tx] (resolve-datom* db e a v tx)
          [begin _end] (apply tuple-range
                              (name index)
                              (take-while
                               some?
                               (rest
                                (tuple-list db
                                            index
                                            [e
                                             a
                                             v
                                             tx]))))
          [_begin end] (tuple-range (name index))]
      (->Eduction
       (comp (map (bytes-to-datoms-xf db))
             (filter (fn [datom]
                       (uuid<= (:tx datom)
                               (:max-tx db))))
             datoms-filter)
       (slice {:db db
               :begin begin
               :end end}))))

  (-rseek-datoms [db index c0 c1 c2 c3]
    (validate-indexed db index c0 c1 c2 c3)
    (let [[e a v tx] (sort-components
                      index
                      [c0 c1 c2 c3])
          [e a v tx] (resolve-datom* db e a v tx)
          start (take-while
                 some?
                 (rest
                  (tuple-list db
                              index
                              [e
                               a
                               v
                               tx])))
          [_begin end] (apply tuple-range
                              (name index)
                              start)
          [begin _end] (tuple-range (name index))]
      (->Eduction
       (comp (map (bytes-to-datoms-xf db))
             (filter (fn [datom]
                       (uuid<= (:tx datom)
                               (:max-tx db))))
             datoms-filter)
       (slice {:db db
               :begin begin
               :end end
               :reverse true}))))

  (-index-range [db attr start end]
    (validate-indexed db :avet attr nil nil nil)
    (validate-attr attr (list '-index-range 'db attr start end))
    (let [[_ _ start*] (resolve-datom* db nil attr start nil)
          [begin _end] (apply tuple-range
                              "avet"
                              (pr-str attr)
                              (when start*
                                [(serialize-value db attr start*)]))
          [_ _ end*] (resolve-datom* db nil attr end nil)
          [_begin end] (apply tuple-range
                              "avet"
                              (pr-str attr)
                              (when end*
                                [(serialize-value db attr end*)]))]
      (->Eduction
       (comp (map (bytes-to-datoms-xf db))
             (filter (fn [datom]
                       (uuid<= (:tx datom)
                               (:max-tx db))))
             datoms-filter)
       (slice {:db db
               :begin begin
               :end end}))))

  clojure.data/EqualityPartition
  (equality-partition [x] :dbval/db)

  clojure.data/Diff
  (diff-similar [a b]
    (diff-sorted (-datoms a
                          :eavt
                          nil
                          nil
                          nil
                          nil)
                 (-datoms b
                          :eavt
                          nil
                          nil
                          nil
                          nil)
                 cmp-datoms-eav-quick)))

(defn db? [x]
  #?(:clj
     (or
       (and x
         (instance? dbval.db.ISearch x)
         (instance? dbval.db.IIndexAccess x)
         (instance? dbval.db.IDB x))
       (and (satisfies? ISearch x)
         (satisfies? IIndexAccess x)
         (satisfies? IDB x)))
     :cljs
     (and (satisfies? ISearch x)
       (satisfies? IIndexAccess x)
       (satisfies? IDB x))))

;; ----------------------------------------------------------------------------
(defrecord-updatable FilteredDB [unfiltered-db pred hash]
  #?@(:cljs
      [IHash                (-hash  [db]        (hash-fdb db))
       IEquiv               (-equiv [db other]  (equiv-db db other))
       ICounted             (-count [db]        (count (-datoms db :eavt nil nil nil nil)))
       IPrintWithWriter     (-pr-writer [db w opts] (pr-db db w opts))

       IEmptyableCollection (-empty [_]         (throw (js/Error. "-empty is not supported on FilteredDB")))

       ILookup              (-lookup ([_ _]     (throw (js/Error. "-lookup is not supported on FilteredDB")))
                              ([_ _ _]   (throw (js/Error. "-lookup is not supported on FilteredDB"))))


       IAssociative         (-contains-key? [_ _] (throw (js/Error. "-contains-key? is not supported on FilteredDB")))
       (-assoc [_ _ _]       (throw (js/Error. "-assoc is not supported on FilteredDB")))]

      :clj
      [Object               (hashCode [db]      (hash-fdb db))

       clojure.lang.IHashEq (hasheq [db]        (hash-fdb db))

       clojure.lang.IPersistentCollection
       (count [db]         (count (-datoms db :eavt nil nil nil nil)))
       (equiv [db o]       (equiv-db db o))
       (cons [db [k v]]    (throw (UnsupportedOperationException. "cons is not supported on FilteredDB")))
       (empty [db]         (throw (UnsupportedOperationException. "empty is not supported on FilteredDB")))

       clojure.lang.ILookup (valAt [db k]       (throw (UnsupportedOperationException. "valAt/2 is not supported on FilteredDB")))
       (valAt [db k nf]    (throw (UnsupportedOperationException. "valAt/3 is not supported on FilteredDB")))
       clojure.lang.IKeywordLookup (getLookupThunk [db k]
                                     (throw (UnsupportedOperationException. "getLookupThunk is not supported on FilteredDB")))

       clojure.lang.Associative
       (containsKey [e k]  (throw (UnsupportedOperationException. "containsKey is not supported on FilteredDB")))
       (entryAt [db k]     (throw (UnsupportedOperationException. "entryAt is not supported on FilteredDB")))
       (assoc [db k v]     (throw (UnsupportedOperationException. "assoc is not supported on FilteredDB")))])

  IDB
  (-schema [db]
    (-schema (.-unfiltered-db db)))

  (-attrs-by [db property]
    (-attrs-by (.-unfiltered-db db) property))

  ISearch
  (-search [db pattern]
    (filter (.-pred db) (-search (.-unfiltered-db db) pattern)))

  IIndexAccess
  (-datoms [db index c0 c1 c2 c3]
    (filter (.-pred db) (-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-seek-datoms [db index c0 c1 c2 c3]
    (filter (.-pred db) (-seek-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-rseek-datoms [db index c0 c1 c2 c3]
    (filter (.-pred db) (-rseek-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-index-range [db attr start end]
    (filter (.-pred db) (-index-range (.-unfiltered-db db) attr start end))))

(defn unfiltered-db ^DB [db]
  (if (instance? FilteredDB db)
    (.-unfiltered-db ^FilteredDB db)
    db))

;; ----------------------------------------------------------------------------

(defn attr->properties [k v]
  (case v
    :db.unique/identity  [:db/unique :db.unique/identity :db/index]
    :db.unique/value     [:db/unique :db.unique/value :db/index]
    :db.cardinality/many [:db.cardinality/many]
    :db.type/ref         [:db.type/ref :db/index]
    (cond
      (and (= :db/isComponent k) (true? v)) [:db/isComponent]
      (and (= :db/index k) (true? v))       [:db/index]
      (= :db/tupleAttrs k)                  [:db.type/tuple :db/index]
      :else [])))

(defn attr-tuples
  "e.g. :reg/semester => #{:reg/semester+course+student ...}"
  [schema rschema]
  (reduce
    (fn [m tuple-attr] ;; e.g. :reg/semester+course+student
      (util/reduce-indexed
        (fn [m src-attr idx] ;; e.g. :reg/semester
          (update m src-attr assoc tuple-attr idx))
        m
        (-> schema (get tuple-attr) :db/tupleAttrs)))
    {}
    (:db.type/tuple rschema)))

(defn- rschema
  ":db/unique           => #{attr ...}
   :db.unique/identity  => #{attr ...}
   :db.unique/value     => #{attr ...}
   :db/index            => #{attr ...}
   :db.cardinality/many => #{attr ...}
   :db.type/ref         => #{attr ...}
   :db/isComponent      => #{attr ...}
   :db.type/tuple       => #{attr ...}
   :db/attrTuples       => {attr => {tuple-attr => idx}}"
  [schema]
  (let [rschema (reduce-kv
                  (fn [rschema attr attr-schema]
                    (reduce-kv
                      (fn [rschema key value]
                        (reduce
                          (fn [rschema prop]
                            (update rschema prop util/conjs attr))
                          rschema (attr->properties key value)))
                      rschema attr-schema))
                  {} schema)]
    (assoc rschema :db/attrTuples (attr-tuples schema rschema))))

(defn- validate-schema-key [a k v expected]
  (when-not (or (nil? v)
              (contains? expected v))
    (throw (ex-info (str "Bad attribute specification for " (pr-str {a {k v}}) ", expected one of " expected)
             {:error :schema/validation
              :attribute a
              :key k
              :value v}))))

(defn- validate-schema [schema]
  (doseq [[a kv] schema]

    ;; isComponent
    (let [comp? (:db/isComponent kv false)]
      (validate-schema-key a :db/isComponent (:db/isComponent kv) #{true false})
      (when (and comp? (not= (:db/valueType kv) :db.type/ref))
        (util/raise "Bad attribute specification for " a ": {:db/isComponent true} should also have {:db/valueType :db.type/ref}"
          {:error     :schema/validation
           :attribute a
           :key       :db/isComponent})))

    (validate-schema-key a :db/unique (:db/unique kv) #{:db.unique/value :db.unique/identity})
    (validate-schema-key a :db/valueType (:db/valueType kv) #{:db.type/ref :db.type/tuple})
    (validate-schema-key a :db/cardinality (:db/cardinality kv) #{:db.cardinality/one :db.cardinality/many})

    ;; tuple should have tupleAttrs
    (when (and (= :db.type/tuple (:db/valueType kv))
            (not (contains? kv :db/tupleAttrs)))
      (util/raise "Bad attribute specification for " a ": {:db/valueType :db.type/tuple} should also have :db/tupleAttrs"
        {:error :schema/validation
         :attribute a
         :key :db/valueType}))

    ;; :db/tupleAttrs is a non-empty sequential coll
    (when (contains? kv :db/tupleAttrs)
      (let [ex-data {:error :schema/validation
                     :attribute a
                     :key :db/tupleAttrs}]
        (when (= :db.cardinality/many (:db/cardinality kv))
          (util/raise a " has :db/tupleAttrs, must be :db.cardinality/one" ex-data))

        (let [attrs (:db/tupleAttrs kv)]
          (when-not (sequential? attrs)
            (util/raise a " :db/tupleAttrs must be a sequential collection, got: " attrs ex-data))

          (when (empty? attrs)
            (util/raise a " :db/tupleAttrs can't be empty" ex-data))

          (doseq [attr attrs
                  :let [ex-data (assoc ex-data :value attr)]]
            (when (contains? (get schema attr) :db/tupleAttrs)
              (util/raise a " :db/tupleAttrs can't depend on another tuple attribute: " attr ex-data))

            (when (= :db.cardinality/many (:db/cardinality (get schema attr)))
              (util/raise a " :db/tupleAttrs can't depend on :db.cardinality/many attribute: " attr ex-data))))))))

(defn execute-sql!
  "Executes a single SQL statement using the given java.sql.Connection.
   Returns true if the statement returned a ResultSet, or false for an update/count.
   Example:
     (execute-sql! conn \"create table if not exists users (id integer primary key, name text)\")"
  [^java.sql.Connection conn ^String sql]
  (with-open [stmt (.createStatement conn)]
    (.execute ^java.sql.Statement stmt sql)))

(defn create-table!
  [^java.sql.Connection conn]
  ;; Create a table
  (execute-sql! conn "create table if not exists dbval (k blob not null, primary key(k)) WITHOUT ROWID;"))

(defn q-max-tx
  [db]
  (let [[begin end] (tuple-range "teav")
        iterator (slice {:db db
                         :begin begin
                         :end end
                         :reverse true})]
    (or (some-> iterator
                (first)
                (tuple-from-bytes)
                (second))
        tx0)))
  
(defn ^DB empty-db [schema opts]
  {:pre [(or (nil? schema) (map? schema))]}
  (validate-schema schema)
  (let [db-file (or (:db-file opts)
                    (.getCanonicalPath
                     (java.io.File/createTempFile (str (random-uuid))
                                                  ".db")))
        _ (io/make-parents db-file)
        ;; TODO: consider how to close the connection:
        conn ^java.sql.Connection (get-sqlite-connection {:db-file db-file})
        _ (create-table! conn)
        _ (.commit conn)
        db (map->DB
            {:schema        schema
             :max-tx        tx0
             :rschema       (rschema (merge implicit-schema schema))
             :db-file       db-file
             :conn          conn
             :pull-patterns (lru/cache 100)
             :pull-attrs    (lru/cache 100)
             :hash          (atom 0)})]
    (assoc db
           :max-tx
           (q-max-tx db))))

(defrecord TxReport [db-before db-after tx-data tempids tx-meta])

(defn datoms->tx
  [datoms]
  (map
   (fn [[e a v tx added]]
     [(if added
        :db/add
        :db/retract)
      e
      a
      v
      tx])
   datoms))

(defn db-transact
  [db tx]
  (:db-after
   (transact-tx-data
    (->TxReport db db [] {} {} ;tx-meta
                )
    tx)))

(defn ^DB init-db [datoms schema opts]
  (when-some [not-datom (first (drop-while datom? datoms))]
    (util/raise "init-db expects list of Datoms, got " (type not-datom)
                {:error :init-db}))
  (validate-schema schema)
  (let [db (empty-db schema opts)]
    (db-transact db
                 (datoms->tx datoms))))

(defn+ ^DB restore-db [{:keys [schema max-tx eavt aevt avet] :as keys}]
  (map->DB
    {:schema        schema
     :max-tx        (or max-tx tx0)
     :rschema       (or (:rschema keys)
                      (rschema (merge implicit-schema schema)))
     :eavt          eavt
     :aevt          aevt
     :avet          avet
     :pull-patterns (lru/cache 100)
     :pull-attrs    (lru/cache 100)
     :hash          (atom 0)}))

(defn with-schema [db schema]
  {:pre [(db? db) (or (nil? schema) (map? schema))]}
  (assoc db
    :schema        schema
    :rschema       (rschema (merge implicit-schema schema))
    :pull-patterns (lru/cache 100)
    :pull-attrs    (lru/cache 100)
    :hash          (atom 0)))

(defn- equiv-db-index [x y]
  (loop [xs (seq x)
         ys (seq y)]
    (cond
      (nil? xs) (nil? ys)
      (= (first xs) (first ys)) (recur (next xs) (next ys))
      :else false)))

(defn+ ^:private ^number hash-db [^DB db]
  (let [h @(.-hash db)]
    (if (zero? h)
      (reset! (.-hash db) (combine-hashes (hash (.-schema db))
                            (hash-unordered-coll (-datoms db :eavt nil nil nil nil))))
      h)))

(defn+ ^:private ^number hash-fdb [^FilteredDB db]
  (let [h @(.-hash db)
        datoms (or (-datoms db :eavt nil nil nil nil) #{})]
    (if (zero? h)
      (let [datoms (or (-datoms db :eavt nil nil nil nil) #{})]
        (reset! (.-hash db) (combine-hashes (hash (-schema db))
                              (hash-unordered-coll datoms))))
      h)))

(defn+ ^:private ^boolean equiv-db [db other]
  (and (or (instance? DB other) (instance? FilteredDB other))
    (= (-schema db) (-schema other))
    (equiv-db-index (-datoms db :eavt nil nil nil nil) (-datoms other :eavt nil nil nil nil))))

#?(:cljs
   (defn+ pr-db [db w opts]
     (-write w "#dbval/DB {")
     (-write w ":schema ")
     (pr-writer (-schema db) w opts)
     (-write w ", :datoms ")
     (pr-sequential-writer w
       (fn [d w opts]
         (pr-sequential-writer w pr-writer "[" " " "]" opts [(.-e d) (.-a d) (.-v d) (datom-tx d)]))
       "[" " " "]" opts (-datoms db :eavt nil nil nil nil))
     (-write w "}")))

#?(:clj
   (do
     (defn pr-db [db, ^java.io.Writer w]
       (.write w (str "#dbval/DB {"))
       (.write w ":schema ")
       (binding [*out* w]
         (pr (-schema db))
         (.write w ", :datoms [")
         (apply pr (map (fn [^Datom d] [(.-e d) (.-a d) (.-v d) (datom-tx d)]) (-datoms db :eavt nil nil nil nil))))
       (.write w "]}"))

     (defmethod print-method DB [db w] (pr-db db w))
     (defmethod print-method FilteredDB [db w] (pr-db db w))))

(defn db-from-reader [{:keys [schema datoms]}]
  (init-db (map (fn [[e a v tx]] (datom e a v tx)) datoms) schema {}))

;; ----------------------------------------------------------------------------

#?(:clj  (declare entid-strict)
   :cljs (defn ^number entid-strict [db eid]))

#?(:clj  (declare ref?)
   :cljs (defn ^boolean ref? [db attr]))

(defn+ resolve-datom [db e a v t default-e default-tx]
  (when (some? a)
    (validate-attr a (list 'resolve-datom 'db e a v t)))
  (datom
    (if (some? e) (entid-strict db e) default-e)
    a
    (if (and (some? v) (ref? db a))
      (entid-strict db v)
      v)
    (if (some? t) (entid-strict db t) default-tx)))

(defn+ components->pattern [db index c0 c1 c2 c3 default-e default-tx]
  (case index
    :eavt (resolve-datom db c0 c1 c2 c3 default-e default-tx)
    :aevt (resolve-datom db c1 c0 c2 c3 default-e default-tx)
    :avet (resolve-datom db c2 c0 c1 c3 default-e default-tx)))

(defn+ resolve-datom* [db e a v t]
  (when (some? a)
    (validate-attr a (list 'resolve-datom 'db e a v t)))
  [(when (some? e) (entid-strict db e))
   a
   (if (and (some? v) (ref? db a))
     (entid-strict db v)
     v)
   (when (some? t) (entid-strict db t))])

(defn+ components->pattern* [db index c0 c1 c2 c3]
  (case index
    :eavt (resolve-datom* db c0 c1 c2 c3)
    :aevt (resolve-datom* db c1 c0 c2 c3)
    :avet (resolve-datom* db c2 c0 c1 c3)))

(defn find-datom [db index c0 c1 c2 c3]
  (validate-indexed db index c0 c1 c2 c3)
  (first (-datoms db index c0 c1 c2 c3)))

;; ----------------------------------------------------------------------------

(defrecord TxReport [db-before db-after tx-data tempids tx-meta])

(defn+ ^boolean is-attr? [db attr property]
  (contains? (-attrs-by db property) attr))

(defn+ ^boolean multival? [db attr]
  (is-attr? db attr :db.cardinality/many))

(defn+ ^boolean multi-value? [db attr value]
  (and
    (is-attr? db attr :db.cardinality/many)
    (or
      (arrays/array? value)
      (and (coll? value) (not (map? value))))))

(defn+ ^boolean ref? [db attr]
  (is-attr? db attr :db.type/ref))

(defn+ ^boolean component? [db attr]
  (is-attr? db attr :db/isComponent))

(defn+ ^boolean indexing? [db attr]
  (is-attr? db attr :db/index))

(defn+ ^boolean tuple? [db attr]
  (is-attr? db attr :db.type/tuple))

(defn+ ^boolean tuple-source? [db attr]
  (is-attr? db attr :db/attrTuples))

(defn+ ^boolean reverse-ref? [attr]
  (cond
    (keyword? attr)
    (= \_ (nth (name attr) 0))
    
    (string? attr)
    (boolean (re-matches #"(?:([^/]+)/)?_([^/]+)" attr))
   
    :else
    (util/raise "Bad attribute type: " attr ", expected keyword or string"
      {:error :transact/syntax, :attribute attr})))

(defn reverse-ref [attr]
  (cond
    (keyword? attr)
    (if (reverse-ref? attr)
      (keyword (namespace attr) (subs (name attr) 1))
      (keyword (namespace attr) (str "_" (name attr))))

    (string? attr)
    (let [[_ ns name] (re-matches #"(?:([^/]+)/)?([^/]+)" attr)]
      (if (= \_ (nth name 0))
        (if ns (str ns "/" (subs name 1)) (subs name 1))
        (if ns (str ns "/_" name) (str "_" name))))
   
    :else
    (util/raise "Bad attribute type: " attr ", expected keyword or string"
      {:error :transact/syntax, :attribute attr})))

(defn resolve-tuple-refs [db a vs]
  (mapv
    (fn [a v]
      (if (and (ref? db a) (sequential? v)) ;; lookup-ref
        (entid-strict db v)
        v))
    (-> db -schema (get a) :db/tupleAttrs) vs))

(defn- tuple-component-values
  "Returns the current component values for entity `e`.
   Used when tuple attrs need to compare queued/DB state during guards and upserts."
  [db e tuple-attrs]
  (mapv
    (fn [attr]
      (:v (first (-datoms db :eavt e attr nil nil))))
    tuple-attrs))

(defn- tuple-existing-entity
  "Looks up an entity that already owns `tuple` with the given component values."
  [db tuple tuple-value]
  (when (is-attr? db tuple :db.unique/identity)
    (let [resolved (resolve-tuple-refs db tuple tuple-value)]
      (:e (first (-datoms db :avet tuple resolved nil nil))))))

(defn- tuple-upsert-eid
  "When temp entity `temp-e` sets tuple component attr `a`, try resolving its tuple target.
   If all tuple components are known (queued or in DB), return the entity that should be upserted."
  [db tempids temp-e a v]
  (when-let [tuples (get (-attrs-by db :db/attrTuples) a)]
    (some
      (fn [[tuple idx]]
        (when (is-attr? db tuple :db.unique/identity)
          (let [tuple-attrs (get-in db [:schema tuple :db/tupleAttrs])
                allocated   (get tempids temp-e)
                components  (map-indexed
                               (fn [i component]
                                 (cond
                                   (= i idx) v
                                   allocated
                                   (:v (first (-datoms db :eavt allocated component nil nil)))
                                   :else nil))
                               tuple-attrs)]
            (when (every? some? components)
              (tuple-existing-entity db tuple components)))))
      tuples)))

(defn+ entid [db eid]
  {:pre [(db? db)]}
  (cond
    (uuid? eid)
    eid

    (sequential? eid)
    (let [[attr value] eid]
      (cond
        (not= (count eid) 2)
        (util/raise "Lookup ref should contain 2 elements: " eid
          {:error :lookup-ref/syntax, :entity-id eid})

        (not (is-attr? db attr :db/unique))
        (util/raise "Lookup ref attribute should be marked as :db/unique: " eid
          {:error :lookup-ref/unique, :entity-id eid})

        (tuple? db attr)
        (let [value' (resolve-tuple-refs db attr value)]
          (-> (-datoms db :avet attr value' nil nil) first :e))

        (nil? value)
        nil

        :else
        (-> (-datoms db :avet attr value nil nil) first :e)))

    #?@(:cljs [(array? eid) (recur db (array-seq eid))])

    (keyword? eid)
    (-> (-datoms db :avet :db/ident eid nil nil) first :e)

    :else
    (util/raise "Expected UUID or lookup ref for entity id, got " eid
      {:error :entity-id/syntax, :entity-id eid})))

(defn+ ^boolean eid-exists? [db eid]
  (= eid (-> (-seek-datoms db :eavt eid nil nil nil) first :e)))

(defn+ ^number entid-strict [db eid]
  (or
    (entid db eid)
    (util/raise "Nothing found for entity id " eid
      {:error :entity-id/missing
       :entity-id eid})))

(defn+ ^number entid-some [db eid]
  (when (some? eid)
    (entid-strict db eid)))

;;;;;;;;;; Transacting

(defn- tempid?
  "Returns true if id is a tempid format (negative integer or string)."
  [id]
  (or (and (integer? id) (neg? id))
      (string? id)))

(defn- find-upsert-id
  "Check if entity has unique identity attributes that resolve to an existing entity.
   Only :db.unique/identity triggers upsert, not :db.unique/value."
  [db entity]
  (when (map? entity)
    (some (fn [[a v]]
            (when (and (keyword? a)
                       (not= a :db/id)
                       ;; Only identity attrs trigger upsert, not value attrs
                       (is-attr? db a :db.unique/identity)
                       (not (nil? v))
                       ;; Skip ref attributes with tempid values - can't look up by unresolved tempid
                       (not (and (ref? db a) (tempid? v))))
              ;; Try to find existing entity with this unique value
              (-> (-datoms db :avet a v nil nil) first :e)))
          entity)))

(defn- find-vector-upserts
  "For vector ops like [:db/add e a v], group by entity and check if their attrs
   match unique identity values that already exist. Returns {:tempid->upsert map, :identity-value->uuid map}.
   Handles both regular unique identity attrs and tuple attrs.
   Throws on conflicting upserts (same tempid resolving to different entities)."
  [db tx-data]
  (let [;; Get all unique identity attrs (including tuples)
        idents (-attrs-by db :db.unique/identity)
        tuples (-attrs-by db :db.type/tuple)
        refs (-attrs-by db :db.type/ref)
        schema (-schema db)
        ;; Group all :db/add vector ops by entity id, collecting ALL values for identity attrs
        ;; For identity attrs, we keep a list of values; for others, just the last value
        adds-by-entity (reduce
                         (fn [acc entity]
                           (if (and (sequential? entity)
                                    (= :db/add (first entity)))
                             (let [[_ e a v] entity]
                               (if (tempid? e)
                                 (if (contains? idents a)
                                   ;; For identity attrs, collect all values as a list
                                   (update-in acc [e a] (fnil conj []) v)
                                   ;; For other attrs, just keep last value
                                   (assoc-in acc [e a] v))
                                 acc))
                             acc))
                         {}
                         tx-data)
        ;; Track within-transaction identity values -> first tempid's UUID
        identity-value->uuid (atom {})
        tempid->upsert (atom {})]
    ;; Process each tempid's attrs
    (doseq [[tempid attrs] adds-by-entity]
      (when-not (contains? @tempid->upsert tempid)
        ;; First check regular unique identity attrs for upserts
        ;; Note: For non-ref attrs, string values are regular values, not tempids
        ;; For ref attrs, skip tempid values (negative ints) as they can't be looked up
        (let [found-upserts
              ;; Collect ALL upserts for this tempid
              (reduce-kv
                (fn [upserts a v-or-vs]
                  (if (and (contains? idents a)
                           (not (contains? tuples a)))
                    ;; v-or-vs is either a vector of values (for identity attrs) or a single value
                    (let [values (if (vector? v-or-vs) v-or-vs [v-or-vs])]
                      (reduce
                        (fn [ups v]
                          (if (and (some? v)
                                   ;; For ref attrs, skip negative int tempids
                                   (not (and (contains? refs a)
                                             (and (integer? v) (neg? v)))))
                            ;; Look up if this value exists in db
                            (if-some [existing (:e (first (-datoms db :avet a v nil nil)))]
                              (conj ups {:attr a :value v :eid existing})
                              ups)
                            ups))
                        upserts
                        values))
                    upserts))
                []
                attrs)]
          ;; Check for conflicting upserts (different existing entities)
          (when (> (count found-upserts) 1)
            (let [distinct-eids (set (map :eid found-upserts))]
              (when (> (count distinct-eids) 1)
                (util/raise "Conflicting upsert: " tempid " resolves both to "
                            (first distinct-eids) " and " (second distinct-eids)
                            {:error :transact/upsert
                             :tempid tempid
                             :upserts found-upserts}))))
          (if (seq found-upserts)
            (swap! tempid->upsert assoc tempid (:eid (first found-upserts)))
            ;; Check tuple attrs
            (let [tuple-upsert
                  (some (fn [tuple-attr]
                          (when (is-attr? db tuple-attr :db.unique/identity)
                            (let [tuple-source-attrs (get-in schema [tuple-attr :db/tupleAttrs])
                                  ;; Get first value for each tuple source attr
                                  tuple-values (mapv #(let [v (get attrs %)]
                                                        (if (vector? v) (first v) v))
                                                     tuple-source-attrs)]
                              ;; All source attrs must be present
                              (when (every? some? tuple-values)
                                ;; Look up if this tuple value exists
                                (when-some [existing (:e (first (-datoms db :avet tuple-attr tuple-values nil nil)))]
                                  existing)))))
                        tuples)]
              (if tuple-upsert
                (swap! tempid->upsert assoc tempid tuple-upsert)
                ;; No db upsert found - check within-transaction duplicates
                (some (fn [[a v-or-vs]]
                        (when (and (contains? idents a)
                                   (not (contains? tuples a)))
                          (let [v (if (vector? v-or-vs) (first v-or-vs) v-or-vs)]
                            (when (some? v)
                              (let [key [a v]]
                                (if-let [existing-uuid (get @identity-value->uuid key)]
                                  ;; Another tempid already claimed this value
                                  (swap! tempid->upsert assoc tempid existing-uuid)
                                  ;; First tempid with this value - generate UUID
                                  (let [new-uuid (random-uuid)]
                                    (swap! tempid->upsert assoc tempid new-uuid)
                                    (swap! identity-value->uuid assoc key new-uuid))))))))
                      attrs)))))))
    {:tempid->upsert @tempid->upsert
     :identity-value->uuid @identity-value->uuid}))

(defn- assign-entity-ids
  "Assigns random UUIDs to entities that don't have a :db/id.
   Converts tempids (negative integers, strings) to UUIDs.
   For entity maps, generates a new UUID if :db/id is missing.
   For vector ops like [:db/add ...], the entity ID must be provided.
   Returns {:tx-data processed-tx-data :id-map tempid->uuid-map}."
  [db tx-data]
  ;; First pass: find all upserts and map tempids to existing entity IDs
  ;; Also detect within-transaction upserts (multiple entities with same unique identity value)
  (let [{vector-upserts :tempid->upsert
         vector-identity-values :identity-value->uuid} (find-vector-upserts db tx-data)
        tempid->upsert (atom vector-upserts)
        ;; Track unique identity values to UUIDs within this transaction
        ;; {[attr value] -> uuid}
        identity-value->uuid (atom vector-identity-values)
        idents (-attrs-by db :db.unique/identity)
        _ (doseq [entity tx-data]
            (when (map? entity)
              (let [old-id (:db/id entity)]
                (when (and (tempid? old-id)
                           (not (contains? @tempid->upsert old-id)))
                  ;; Check if it upserts to an existing entity in db
                  (if-let [upsert-id (find-upsert-id db entity)]
                    (swap! tempid->upsert assoc old-id upsert-id)
                    ;; Check if another entity in this transaction has the same unique identity value
                    (some (fn [[a v]]
                            (when (and (contains? idents a) (some? v))
                              (let [key [a v]]
                                (if-let [existing-uuid (get @identity-value->uuid key)]
                                  ;; Another entity already claimed this value - map to it
                                  (swap! tempid->upsert assoc old-id existing-uuid)
                                  ;; First entity with this value - generate UUID and record it
                                  (let [new-uuid (random-uuid)]
                                    (swap! tempid->upsert assoc old-id new-uuid)
                                    (swap! identity-value->uuid assoc key new-uuid))))))
                          entity))))))
        ;; Use an atom to track tempid -> UUID mappings within this transaction
        id-map (atom {})]
    (letfn [(resolve-id [id]
              (cond
                (uuid? id) id
                (sequential? id) id  ;; lookup ref, keep as-is
                (keyword? id) id     ;; :db/ident or :db/current-tx
                ;; tx-id string aliases must pass through to be resolved later
                (and (string? id) (or (= id "datomic.tx") (= id "dbval.tx"))) id
                (tempid? id)
                (or
                  ;; Check if this tempid upserts to an existing entity
                  (get @tempid->upsert id)
                  ;; Otherwise use cached mapping or generate new UUID
                  (if-let [uuid (get @id-map id)]
                    uuid
                    (let [uuid (random-uuid)]
                      (swap! id-map assoc id uuid)
                      uuid)))
                :else id))
            (process-entity [entity]
              (util/cond+
                (map? entity)
                (let [old-id (:db/id entity)
                      new-id (if (contains? entity :db/id)
                               (resolve-id old-id)
                               (random-uuid))]
                  (reduce-kv
                    (fn [entity a v]
                      (cond
                        (not (or (keyword? a) (string? a)))
                        (assoc entity a v)

                        ;; Multi-value ref with collection of values (not a single lookup ref)
                        (and (ref? db a) (multi-value? db a v) (not (keyword? (first v))))
                        (assoc entity a
                          (mapv (fn [elem]
                                  (cond
                                    (map? elem) (process-entity elem)
                                    (or (uuid? elem) (sequential? elem) (keyword? elem)) elem
                                    :else (resolve-id elem)))
                                v))

                        (ref? db a)
                        (if (map? v)
                          ;; Nested entity map - process it recursively
                          (assoc entity a (process-entity v))
                          ;; ID reference - resolve if needed
                          (let [resolved (if (or (uuid? v) (sequential? v) (keyword? v))
                                           v
                                           (resolve-id v))]
                            (assoc entity a resolved)))

                        (and (reverse-ref? a) (sequential? v) (keyword? (first v)))
                        ;; Lookup ref like [:name "Ivan"] - keep as-is
                        (assoc entity a v)

                        (and (reverse-ref? a) (sequential? v))
                        ;; Collection of refs like ["tempid1" "tempid2"]
                        (assoc entity a
                          (mapv (fn [elem]
                                  (cond
                                    (map? elem) (process-entity elem)
                                    (or (uuid? elem) (sequential? elem) (keyword? elem)) elem
                                    :else (resolve-id elem)))
                                v))

                        (reverse-ref? a)
                        (if (map? v)
                          (assoc entity a (process-entity v))
                          (assoc entity a (if (or (uuid? v) (sequential? v) (keyword? v))
                                            v
                                            (resolve-id v))))

                        :else
                        (assoc entity a v)))
                    {:db/id new-id}
                    (dissoc entity :db/id)))

                ;; :db.fn/call entities pass through unchanged - they don't have normal entity IDs
                (and (sequential? entity) (= :db.fn/call (first entity)))
                entity

                ;; :db.fn/cas and :db/cas have 5 elements - need to preserve all of them
                (and (sequential? entity) (#{:db.fn/cas :db/cas} (first entity)))
                (let [[op e a ov nv] entity]
                  [op (resolve-id e) a ov nv])

                (and
                  (sequential? entity)
                  :let [[op e a v] entity]
                  (keyword? op))
                (let [new-e (resolve-id e)]
                  (cond
                    ;; Multi-value ref with collection of values (not a single lookup ref)
                    (and (= :db/add op) (ref? db a) (multi-value? db a v) (not (keyword? (first v))))
                    [op new-e a (mapv #(cond
                                         (map? %) (process-entity %)
                                         (or (uuid? %) (sequential? %) (keyword? %)) %
                                         :else (resolve-id %)) v)]

                    (and (= :db/add op) (ref? db a) (map? v))
                    [op new-e a (process-entity v)]

                    (and (= :db/add op) (ref? db a))
                    [op new-e a (if (or (uuid? v) (sequential? v) (keyword? v)) v (resolve-id v))]

                    :else
                    [op new-e a v]))

                :else
                entity))]
      {:tx-data (mapv process-entity tx-data)
       ;; Merge upsert mappings with new ID mappings
       :id-map (merge @tempid->upsert @id-map)})))

(defn validate-datom [db ^Datom datom]
  (when (and (datom-added datom)
          (is-attr? db (.-a datom) :db/unique))
    (when-some [found (not-empty (-datoms db :avet (.-a datom) (.-v datom) nil nil))]
      (util/raise "Cannot add " datom " because of unique constraint: " found
        {:error :transact/unique
         :attribute (.-a datom)
         :datom datom}))))

(defn- current-tx
  "Returns the transaction ID (squuid) for this transaction.
   Generated once at transaction start and cached in ::tx-id."
  [report]
  (::tx-id report))

(defn- next-eid
  "Generates a new random UUID for an entity."
  [_db]
  (random-uuid))

#?(:clj
   (defn- ^Boolean tx-id?
     [e]
     (or (identical? :db/current-tx e)
       (.equals ":db/current-tx" e) ;; for dbval.js interop
       (.equals "datomic.tx" e)
       (.equals "dbval.tx" e)))

   :cljs
   (defn- ^boolean tx-id?
     [e]
     (or (= e :db/current-tx)
       (= e ":db/current-tx") ;; for dbval.js interop
       (= e "datomic.tx")
       (= e "dbval.tx"))))

(defn- allocate-eid
  "Simplified allocate-eid for UUID-based IDs.
   Only tracks :db/current-tx in tempids map."
  ([report _eid]
   report)  ; No-op, UUIDs don't need tracking
  ([report e eid]
   (cond-> report
     (tx-id? e)
     (update :tempids assoc e eid))))

;; In context of `with-datom` we can use faster comparators which
;; do not check for nil (~10-15% performance gain in `transact`)

(defn retract-datom
  [datom* tx]
  (datom (:e datom*)
         (:a datom*)
         (:v datom*)
         tx
         false))

(defn set-add!
  [db stmt tuple]
  (try
    (.setBytes ^java.sql.PreparedStatement stmt
               1
               (pack tuple))
    (.addBatch ^java.sql.PreparedStatement stmt)
    (catch Exception e
      (throw (ex-info "set-add! failed"
                      {:tuple tuple}
                      e))))
  db)

(defn all-tuples
  "Returns a reducible of all tuples stored in the dbval table.
   Each tuple is decoded from its byte representation.
   Useful for debugging and inspecting the raw storage.
   Example: (into [] (take 10) (all-tuples db))"
  [db]
  (reify clojure.lang.IReduceInit
    (reduce [_ rf init]
      (let [conn ^java.sql.Connection (:conn db)]
        (with-open [stmt (.prepareStatement conn "SELECT k FROM dbval ORDER BY k")
                    rs (.executeQuery stmt)]
          (loop [state init]
            (if (or (reduced? state) (not (.next rs)))
              (unreduced state)
              (recur (rf state (vec (tuple-from-bytes (.getBytes rs "k"))))))))))))

(defn with-datom [db ^Datom datom]
  (validate-datom db datom)
  (let [conn ^java.sql.Connection (:conn db)
        stmt ^java.sql.PreparedStatement (.prepareStatement
                                           conn
                                           "INSERT OR IGNORE INTO dbval (k) VALUES (?)")
        indexing? (indexing? db (.-a datom))
        db (if (datom-added datom)
             (-> db
                 (set-add! stmt (datom-tuple db :eavt datom))
                 (set-add! stmt (datom-tuple db :aevt datom))
                 (cond-> indexing? (set-add! stmt (datom-tuple db :avet datom)))
                 (set-add! stmt (datom-tuple db :teav datom))
                 (assoc :hash (atom 0)))
             (if-some [removing (some-> (fsearch db [(.-e datom) (.-a datom) (.-v datom)])
                                        (retract-datom (:tx datom)))]
               (-> db
                   (set-add! stmt (datom-tuple db :eavt removing))
                   (set-add! stmt (datom-tuple db :aevt removing))
                   (cond-> indexing? (set-add! stmt (datom-tuple db :avet removing)))
                   (set-add! stmt (datom-tuple db :teav removing))
                   (assoc :hash (atom 0)))
               db))]
    (.executeBatch stmt)
    (.close stmt)
    db))

(defn- queue-tuple [queue tuple idx db e a v]
  (let [tuple-attrs    (-> db (-schema) (get tuple) :db/tupleAttrs)
        empty-value    (vec (repeat (count tuple-attrs) nil))
        db-value       (:v (first (-datoms db :eavt e tuple nil nil)))
        components     (delay (tuple-component-values db e tuple-attrs))
        with-fallback  (fn [value]
                        (or value
                          @components
                          empty-value))
        tuple-value    (if-let [queued (get queue tuple)]
                         (let [fallback (with-fallback db-value)]
                           (mapv (fn [queued-val fallback-val]
                                   (if (nil? queued-val) fallback-val queued-val))
                             queued fallback))
                         (with-fallback db-value))
        tuple-value'   (assoc tuple-value idx v)]
    (assoc queue tuple tuple-value')))

(defn- queue-tuples [queue tuples db e a v]
  (reduce-kv
    (fn [queue tuple idx]
      (queue-tuple queue tuple idx db e a v))
    queue
    tuples))

(defn- transact-report [report datom]
  (let [db      (:db-after report)
        a       (:a datom)
        report' (-> report
                  (assoc :db-after (with-datom db datom))
                  (update :tx-data conj datom))]
    (if (tuple-source? db a)
      (let [e      (:e datom)
            v      (if (datom-added datom) (:v datom) nil)
            queue  (or (-> report' ::queued-tuples (get e)) {})
            tuples (get (-attrs-by db :db/attrTuples) a)
            queue' (queue-tuples queue tuples db e a v)]
        (update report' ::queued-tuples assoc e queue'))
      report')))

(defn- resolve-upserts
  "Returns [entity' upserts]. Upsert attributes that resolve to existing entities
   are removed from entity, rest are kept in entity for insertion. No validation is performed.

   upserts :: {:name  {\"Ivan\"  1}
               :email {\"ivan@\" 2}
               :alias {\"abc\"   3
                       \"def\"   4}}}"
  [db entity]
  (if-some [idents (not-empty (-attrs-by db :db.unique/identity))]
    (let [resolve (fn [a v]
                    ;; Resolve lookup refs in values (for refs and tuples)
                    (let [v (cond
                              (tuple? db a)
                              (resolve-tuple-refs db a v)
                              (ref? db a)
                              (entid db v)
                              :else v)]
                      (:e (first (-datoms db :avet a v nil nil)))))
          split   (fn [a vs]
                    (reduce
                      (fn [acc v]
                        (if-some [e (resolve a v)]
                          (update acc 1 assoc v e)
                          (update acc 0 conj v)))
                      [[] {}] vs))]
      (let [[entity' upserts]
            (reduce-kv
              (fn [[entity' upserts] a v]
                (validate-attr a entity)
                (validate-val v entity)
                (cond
                  (not (contains? idents a))
                  [(assoc entity' a v) upserts]

                  (multi-value? db a v)
                  (let [[insert upsert] (split a v)]
                    [(cond-> entity'
                       (not (empty? insert)) (assoc a insert))
                     (cond-> upserts
                       (not (empty? upsert)) (assoc a upsert))])

                  :else
                  (if-some [e (resolve a v)]
                    [entity' (assoc upserts a {v e})]
                    [(assoc entity' a v) upserts])))
              [{} {}]
              entity)
            schema (-schema db)
            upserts' (reduce
                       (fn [upserts tuple]
                         (if (is-attr? db tuple :db.unique/identity)
                           (let [tuple-attrs (get-in schema [tuple :db/tupleAttrs])
                                 values      (mapv entity tuple-attrs)]
                             (if (every? some? values)
                               (if-some [existing (tuple-existing-entity db tuple values)]
                                 (update upserts tuple assoc values existing)
                                 upserts)
                               upserts))
                           upserts))
                       upserts
                       (-attrs-by db :db.type/tuple))]
        [entity' (not-empty upserts')]))
    [entity nil]))

(defn validate-upserts
  "Throws if not all upserts point to the same entity.
   Returns single eid that all upserts point to, or null."
  [db entity upserts]
  (let [upsert-ids (reduce-kv
                     (fn [m a v->e]
                       (reduce-kv
                         (fn [m v e]
                           (assoc m e [a v]))
                         m v->e))
                     {} upserts)]
    (if (<= 2 (count upsert-ids))
      (let [[e1 [a1 v1]] (first upsert-ids)
            [e2 [a2 v2]] (second upsert-ids)]
        (util/raise "Conflicting upserts: " [a1 v1] " resolves to " e1 ", but " [a2 v2] " resolves to " e2
          {:error     :transact/upsert
           :assertion [e1 a1 v1]
           :conflict  [e2 a2 v2]}))
      (let [[upsert-id [a v]] (first upsert-ids)
            eid (:db/id entity)]
        (when (and
                (some? upsert-id)
                (some? eid)
                (not= upsert-id eid)
                ;; Only error if eid is an existing entity in the database.
                ;; If eid was assigned from a tempid and doesn't exist yet,
                ;; the upsert takes precedence.
                (some? (fsearch db [eid])))
          (util/raise "Conflicting upsert: " [a v] " resolves to " upsert-id ", but entity already has :db/id " eid
            {:error     :transact/upsert
             :assertion [upsert-id a v]
             :conflict  {:db/id eid}}))
        upsert-id))))

;; multivals/reverse can be specified as coll or as a single value, trying to guess
(defn- maybe-wrap-multival [db a vs]
  (cond
    ;; not a multival context
    (not (or (reverse-ref? a)
           (multival? db a)))
    [vs]

    ;; not a collection at all, so definitely a single value
    (not (or (arrays/array? vs)
           (and (coll? vs) (not (map? vs)))))
    [vs]
    
    ;; probably lookup ref
    (and (= (count vs) 2)
      (is-attr? db (first vs) :db.unique/identity))
    [vs]
    
    :else vs))

(defn- explode [db entity]
  (let [eid  (:db/id entity)
        ;; sort tuple attrs after non-tuple
        a+vs (apply concat
               (reduce
                 (fn [acc [a vs]]
                   (update acc (if (tuple? db a) 1 0) conj [a vs]))
                 [[] []] entity))]
    (for [[a vs] a+vs
          :when  (not= a :db/id)
          :let   [_          (validate-attr a {:db/id eid, a vs})
                  reverse?   (reverse-ref? a)
                  straight-a (if reverse? (reverse-ref a) a)
                  _          (when (and reverse? (not (ref? db straight-a)))
                               (util/raise "Bad attribute " a ": reverse attribute name requires {:db/valueType :db.type/ref} in schema"
                                 {:error :transact/syntax, :attribute a, :context {:db/id eid, a vs}}))]
          v      (maybe-wrap-multival db a vs)]
      (if (and (ref? db straight-a) (map? v)) ;; another entity specified as nested map
        (assoc v (reverse-ref a) eid)
        (if reverse?
          [:db/add v   straight-a eid]
          [:db/add eid straight-a v])))))

(defn- transact-add [report [_ e a v tx :as ent]]
  (validate-attr a ent)
  (validate-val  v ent)
  (let [tx        (or tx (current-tx report))
        db        (:db-after report)
        e         (entid-strict db e)
        v         (if (ref? db a) (entid-strict db v) v)
        new-datom (datom e a v tx)
        multival? (multival? db a)
        old-datom ^Datom (if multival?
                           (fsearch db [e a v])
                           (fsearch db [e a]))]
    (cond
      (nil? old-datom)
      (transact-report report new-datom)

      (= (.-v old-datom) v)
      (update report ::tx-redundant util/conjv new-datom)

      :else
      (-> report
        (transact-report (datom e a (.-v old-datom) tx false))
        (transact-report new-datom)))))

(defn- transact-retract-datom [report ^Datom d]
  (let [tx (current-tx report)]
    (transact-report report (datom (.-e d) (.-a d) (.-v d) tx false))))

(defn- retract-components [db datoms]
  (into #{} (comp
              (filter (fn [^Datom d] (component? db (.-a d))))
              (map (fn [^Datom d] [:db.fn/retractEntity (.-v d)]))) datoms))

#?(:clj  (declare transact-tx-data-impl)
   :cljs (defn transact-tx-data-impl [initial-report initial-es]))

#?(:clj
   (defn- rollback-report!
     "Rolls the JDBC connection back to the transaction start before a retry.
      Ensures tuple writes and schema mutations are cleared prior to re-running."
     [report]
     (when-let [conn (some-> report :db-after :conn)]
       (try
         (.rollback ^java.sql.Connection conn)
         (.setAutoCommit ^java.sql.Connection conn false)
         (catch Throwable t
           (throw (ex-info "Failed to rollback transaction for retry"
                           {:error :transact/retry}
                           t))))))
   :cljs
   (defn ^:private rollback-report!
     "No-op placeholder for CLJS where transactions are in-memory."
     [report]
     report))

(def builtin-fn?
  #{:db.fn/call
    :db.fn/cas
    :db/cas
    :db/add
    :db/retract
    :db.fn/retractAttribute
    :db.fn/retractEntity
    :db/retractEntity})

(defn flush-tuples [report]
  (let [db (:db-after report)]
    (reduce-kv
      (fn [entities eid tuples+values]
        (reduce-kv
          (fn [entities tuple value]
            (let [value   (if (every? nil? value) nil value)
                  current (:v (first (-datoms db :eavt eid tuple nil nil)))]
              (cond
                (= value current) entities
                (nil? value)      (conj entities ^::internal [:db/retract eid tuple current])
                :else             (conj entities ^::internal [:db/add eid tuple value]))))
          entities
          tuples+values))
      []
      (::queued-tuples report))))

(defn check-value-tempids [report]
  ;; With UUID-based IDs, tempid tracking is no longer needed.
  ;; Just clean up internal keys.
  (dissoc report ::tx-redundant))

(defn+ transact-tx-data-impl [initial-report initial-es]
  (let [initial-report' initial-report
        has-tuples?     (not (empty? (-attrs-by (:db-after initial-report) :db.type/tuple)))
        initial-es'     (if has-tuples?
                          (interleave initial-es (repeat ::flush-tuples))
                          initial-es)]
    (loop [report initial-report'
           es     initial-es']
      (util/log "transact" es)
      (util/cond+
        (empty? es)
        (-> report
          (update :db-after assoc :max-tx (current-tx report))
          (update :tempids assoc :db/current-tx (current-tx report)))

        :let [[entity & entities] es]

        (nil? entity)
        (recur report entities)

        (= ::flush-tuples entity)
        (if (contains? report ::queued-tuples)
          (recur
            (dissoc report ::queued-tuples)
            (concat (flush-tuples report) entities))
          (recur report entities))

        :let [db      (:db-after report)
              tempids (:tempids report)]

        (map? entity)
        (let [old-eid (:db/id entity)]
          (util/cond+
            ;; trivial entity
            ; (if (contains? entity :db/id)
            ;   (= 1 (count entity))
            ;   (= 0 (count entity)))
            ; (recur report entities)

            ;; :db/current-tx / "datomic.tx" => tx
            (tx-id? old-eid)
            (let [id (current-tx report)
                  ;; Check if any unique identity values would upsert to a different entity
                  upsert-id (find-upsert-id db entity)]
              (when (some? upsert-id)
                (let [conflict-attr (some (fn [[a v]]
                                            (when (and (keyword? a)
                                                       (not= a :db/id)
                                                       (is-attr? db a :db.unique/identity)
                                                       (some? v))
                                              [a v]))
                                          entity)]
                  (util/raise "Conflicting upsert: " conflict-attr " resolves to " upsert-id
                              ", but entity already has :db/id " old-eid
                              {:error :transact/upsert
                               :assertion [upsert-id (first conflict-attr) (second conflict-attr)]
                               :conflict {:db/id old-eid}})))
              (recur (allocate-eid report old-eid id)
                (cons (assoc entity :db/id id) entities)))
           
            ;; lookup-ref => resolved | error
            (sequential? old-eid)
            (let [id (entid-strict db old-eid)]
              (recur report
                (cons (assoc entity :db/id id) entities)))
           
            ;; upserted => explode | error
            :let [[entity' upserts] (resolve-upserts db entity)
                  upserted-eid      (validate-upserts db entity' upserts)]

            (some? upserted-eid)
            (recur
              (-> report
                (allocate-eid old-eid upserted-eid))
              (concat (explode db (assoc entity' :db/id upserted-eid)) entities))
           
            ;; UUID or nil => explode
            (or
              (uuid? old-eid)
              (nil? old-eid))
            (recur report (concat (explode db entity) entities))
           
            ;; trash => error
            :else
            (util/raise "Expected UUID or lookup ref for :db/id, got " old-eid
              {:error :entity-id/syntax, :entity entity})))

        (sequential? entity)
        (let [[op e a v] entity]
          (util/cond+
            (= op :db.fn/call)
            (let [[_ f & args] entity
                  fn-result (apply f db args)
                  {:keys [tx-data id-map]} (assign-entity-ids db fn-result)]
              (recur (update report :tempids merge id-map) (concat tx-data entities)))
            
            (and (keyword? op)
              (not (builtin-fn? op)))
            (if-some [ident (entid db op)]
              (let [fun  (:v (fsearch db [ident :db/fn]))
                    args (next entity)]
                (if (fn? fun)
                  (let [{:keys [tx-data id-map]} (assign-entity-ids db (apply fun db args))]
                    (recur (update report :tempids merge id-map) (concat tx-data entities)))
                  (util/raise "Entity " op " expected to have :db/fn attribute with fn? value"
                    {:error :transact/syntax, :operation :db.fn/call, :tx-data entity})))
              (util/raise "Can't find entity for transaction fn " op
                {:error :transact/syntax, :operation :db.fn/call, :tx-data entity}))
            
            (or (= op :db.fn/cas)
              (= op :db/cas))
            (let [[_ e a ov nv] entity
                  e      (entid-strict db e)
                  _      (validate-attr a entity)
                  ov     (if (ref? db a) (entid-strict db ov) ov)
                  nv     (if (ref? db a) (entid-strict db nv) nv)
                  _      (validate-val nv entity)
                  datoms (vec (-search db [e a]))]
              (if (multival? db a)
                (if (some (fn [^Datom d] (= (.-v d) ov)) datoms)
                  (recur (transact-add report [:db/add e a nv]) entities)
                  (util/raise ":db.fn/cas failed on datom [" e " " a " " (map :v datoms) "], expected " ov
                    {:error :transact/cas, :old datoms, :expected ov, :new nv}))
                (let [v (:v (first datoms))]
                  (if (= v ov)
                    (recur (transact-add report [:db/add e a nv]) entities)
                    (util/raise ":db.fn/cas failed on datom [" e " " a " " v "], expected " ov
                      {:error :transact/cas, :old (first datoms), :expected ov, :new nv})))))

            (tx-id? e)
            (recur (allocate-eid report e (current-tx report)) (cons [op (current-tx report) a v] entities))

            (and (ref? db a) (tx-id? v))
            (recur (allocate-eid report v (current-tx report)) (cons [op e a (current-tx report)] entities))

            ;; Resolve lookup refs in entity position for :db/add
            ;; For retract ops, we use entid (not entid-strict) so missing refs become no-ops
            (and (= op :db/add) (sequential? e) (keyword? (first e)))
            (let [resolved-e (entid-strict db e)]
              (recur report (cons [op resolved-e a v] entities)))

            (and
              (or (= op :db/add) (= op :db/retract))
              (not (::internal (meta entity)))
              (tuple? db a)
              :let [v' (resolve-tuple-refs db a v)]
              (not= v v'))
            (recur report (cons [op e a v'] entities))

            ;; Upsert check: when adding a unique identity value that already exists on another entity
            ;; and the current entity doesn't exist yet, redirect to the existing entity
            (and
              (= op :db/add)
              (is-attr? db a :db.unique/identity)
              :let [existing-eid (:e (first (-datoms db :avet a v nil nil)))]
              existing-eid
              (not= existing-eid e)
              :let [e-exists? (seq (-search db [e]))]
              (not e-exists?))
            ;; Upsert: redirect this entity to the existing one
            (recur (allocate-eid report e existing-eid) (cons [op existing-eid a v] entities))
            
            (and
              (not (::internal (meta entity)))
              (tuple? db a))
            ;; allow transacting in tuples if they fully match already existing values
            (let [tuple-attrs   (get-in db [:schema a :db/tupleAttrs])
                  queued-value (get-in report [::queued-tuples e a])]
              (if queued-value
                (if (and
                      (= (count tuple-attrs) (count queued-value) (count v))
                      (every? some? queued-value)
                      (= v queued-value))
                  (recur report entities)
                  (util/raise "Can't modify tuple attrs directly: " entity
                    {:error :transact/syntax, :tx-data entity}))
                (let [component-values (tuple-component-values db e tuple-attrs)
                      prev-values      (when-some [db-before (:db-before report)]
                                         (tuple-component-values db-before e tuple-attrs))
                      effective-values (if prev-values
                                         (mapv (fn [curr prev]
                                                 (if (nil? curr) prev curr))
                                           component-values prev-values)
                                         component-values)]
                  (if (and
                        (= (count tuple-attrs) (count v))
                        (every? some? v)
                        (every?
                          (fn [[tuple-value component-value]]
                            (= tuple-value component-value))
                          (map vector v effective-values)))
                    (recur report entities)
                    (util/raise "Can't modify tuple attrs directly: " entity
                      {:error :transact/syntax, :tx-data entity})))))

            (= op :db/add)
            (recur (transact-add report entity) entities)

            (and (= op :db/retract) (some? v))
            (if-some [e (entid db e)]
              (let [v (if (ref? db a) (entid-strict db v) v)]
                (validate-attr a entity)
                (validate-val v entity)
                (if-some [old-datom (fsearch db [e a v])]
                  (recur (transact-retract-datom report old-datom) entities)
                  (recur report entities)))
              (recur report entities))

            (or (= op :db.fn/retractAttribute)
              (= op :db/retract))
            (if-some [e (entid db e)]
              (let [_      (validate-attr a entity)
                    datoms (vec (-search db [e a]))]
                (recur (reduce transact-retract-datom report datoms)
                  (concat (retract-components db datoms) entities)))
              (recur report entities))

            (or (= op :db.fn/retractEntity)
              (= op :db/retractEntity))
            (if-some [e (entid db e)]
              (let [e-datoms (vec (-search db [e]))
                    v-datoms (vec (mapcat (fn [a] (-search db [nil a e])) (-attrs-by db :db.type/ref)))]
                (recur (reduce transact-retract-datom report (concat e-datoms v-datoms))
                  (concat (retract-components db e-datoms) entities)))
              (recur report entities))

            :else
            (util/raise "Unknown operation at " entity ", expected :db/add, :db/retract, :db.fn/call, :db.fn/retractAttribute, :db.fn/retractEntity or an ident corresponding to an installed transaction function (e.g. {:db/ident <keyword> :db/fn <Ifn>}, usage of :db/ident requires {:db/unique :db.unique/identity} in schema)" {:error :transact/syntax, :operation op, :tx-data entity})))
       
        (datom? entity)
        (let [[e a v tx added] entity]
          (if added
            (recur (transact-add report [:db/add e a v tx]) entities)
            (recur report (cons [:db/retract e a v] entities))))

        :else
        (util/raise "Bad entity type at " entity ", expected map or vector"
          {:error :transact/syntax, :tx-data entity})))))

(defmacro with-transaction
  "Run BODY within a JDBC transaction on CONN.
   - If CONN is already in a transaction (autocommit=false), just join it.
   - If autocommit=true, disable it, start a transaction, and commit/rollback at the end."
  [^java.sql.Connection conn & body]
  `(let [was-auto?# (.getAutoCommit ~conn)]
     (if-not was-auto?#
       ;; Already inside a transaction — just run the body
       (do ~@body)
       ;; Start and manage a new transaction
       (do
         (.setAutoCommit ~conn false)
         (try
           (let [res# (do ~@body)]
             (.commit ~conn)
             res#)
           (catch Throwable t#
             (try (.rollback ~conn) (catch Throwable _#))
             (throw t#))
           (finally
             (.setAutoCommit ~conn true)))))))

(defn transact-tx-data [report es]
  (when-not (or
              (nil? es)
              (sequential? es))
    (util/raise "Bad transaction data " es ", expected sequential collection"
      {:error :transact/syntax, :tx-data es}))
  (let [tx-id (squuid/generate-squuid)
        report' (-> report
                    (assoc ::tx-id tx-id)
                    ;; Set max-tx to current tx-id so datoms added during this
                    ;; transaction are visible when searching for duplicates
                    (update :db-after assoc :max-tx tx-id))
        {:keys [tx-data id-map]} (assign-entity-ids (:db-before report') es)
        ;; Pre-populate tempids with the tempid -> UUID mapping
        report'' (update report' :tempids merge id-map)
        conn ^java.sql.Connection (:conn (:db-after report''))]
    (try
      (let [result (with-transaction conn
                     (transact-tx-data-impl report'' tx-data))]
        (.commit conn)
        ;; Add :tx field with the transaction UUID
        (assoc result :tx tx-id))
      (catch Throwable t
        (try
          (.rollback conn)
          (.setAutoCommit conn false)
          (catch Throwable _))
        (throw t)))))
