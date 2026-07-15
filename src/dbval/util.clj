(ns dbval.util
  (:refer-clojure :exclude [find])
  )

(def ^:dynamic *debug*
  false)

(defmacro log [& body]
     (when (System/getProperty "dbval.debug")
       `(when *debug*
          (println ~@body))))

(defmacro raise [& fragments]
     (let [msgs (butlast fragments)
           data (last fragments)]
       `(throw (ex-info (str ~@(map (fn [m#] (if (string? m#) m# (list 'pr-str m#))) msgs)) ~data))))

(def ^:private ^:dynamic *if+-syms)

(defn- if+-rewrite-cond-impl [cond]
     (clojure.core/cond
       (empty? cond)
       true

       (and
         (= :let (first cond))
         (empty? (second cond)))
       (if+-rewrite-cond-impl (nnext cond))

       (= :let (first cond))
       (let [[var val & rest] (second cond)
             sym                (gensym)]
         (vswap! *if+-syms conj [var sym])
         (list 'let [var (list 'clojure.core/vreset! sym val)]
           (if+-rewrite-cond-impl
             (cons 
               :let
               (cons rest
                 (nnext cond))))))

       :else
       (list 'and
         (first cond)
         (if+-rewrite-cond-impl (next cond)))))

(defn- if+-rewrite-cond [cond]
     (binding [*if+-syms (volatile! [])]
       [(if+-rewrite-cond-impl cond) @*if+-syms]))

(defn- flatten-1 [xs]
     (vec
       (mapcat identity xs)))

(defmacro if+
     "Allows sharing local variables between condition and then clause.

      Use `:let [...]` form (not nested!) inside `and` condition and its bindings
      will be visible in later `and` clauses and inside `then` branch:

        (if+ (and
               (= 1 2)
               ;; same :let syntax as in doseq/for
               :let [x 3
                     y (+ x 4)]
               ;; x and y visible downstream
               (> y x))

          ;; then: x and y visible here!
          (+ x y 5)

          ;; else: no x or y
          6)"
     [cond then else]
     (if (and
           (seq? cond)
           (or
             (= 'and (first cond))
             (= 'clojure.core/and (first cond))))
       (let [[cond' syms] (if+-rewrite-cond (next cond))]
         `(let ~(flatten-1
                  (for [[_ sym] syms]
                    [sym '(volatile! nil)]))
            (if ~cond'
              (let ~(flatten-1
                      (for [[binding sym] syms]
                        [binding (list 'deref sym)]))
                ~then)
              ~else)))
       (list 'if cond then else)))

(defmacro cond+ [& clauses]
     (when-some [[test expr & rest] clauses]
       (case test
         :do   `(do ~expr (util/cond+ ~@rest))
         :let  `(let ~expr (util/cond+ ~@rest))
         :some `(or ~expr (util/cond+ ~@rest))
         `(util/if+ ~test ~expr (util/cond+ ~@rest)))))

(defmacro some-of
     ([] nil)
     ([x] x)
     ([x & more]
      `(let [x# ~x] (if (nil? x#) (some-of ~@more) x#))))

(defn distinct-by [f coll]
  (->>
    (reduce
      (fn [[seen res :as acc] el]
        (let [key (f el)]
          (if (contains? seen key)
            acc
            [(conj! seen key) (conj! res el)])))
      [(transient #{}) (transient [])]
      coll)
    second
    persistent!))

(defn find [pred xs]
  (reduce
    (fn [_ x]
      (when (pred x)
        (reduced x)))
    nil xs))

(defn single [coll]
  (assert (nil? (next coll)) "Expected single element")
  (first coll))

(defn concatv [& xs]
  (into [] cat xs))

(defn zip
  ([a b]
   (mapv vector a b))
  ([a b & rest]
   (apply mapv vector a b rest)))

(defn removem [key-pred m]
  (persistent!
    (reduce-kv
      (fn [m k v]
        (if (key-pred k)
          m
          (assoc! m k v)))
      (transient (empty m)) m)))

(def conjv
  (fnil conj []))

(def conjs
  (fnil conj #{}))

(defn reduce-indexed
  "Same as reduce, but `f` takes [acc el idx]"
  [f init xs]
  (first
    (reduce
      (fn [[acc idx] x]
        (let [res (f acc x idx)]
          (if (reduced? res)
            (reduced [res idx])
            [res (inc idx)])))
      [init 0]
      xs)))
