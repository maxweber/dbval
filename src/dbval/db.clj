(ns ^:no-doc dbval.db
  (:require

    [clojure.data]
    [clojure.edn :as edn]
    [dbval.inline :refer [update]]
    [dbval.lru :as lru]
    [dbval.store :as store]
    [dbval.tuple :as tuple-codec]
    [dbval.util :as util]
    [dbval.arrays :as arrays]
    [com.yetanalytics.squuid :as squuid]
    [com.yetanalytics.squuid.uuid :as squuid-uuid])

  (:refer-clojure :exclude [seqable? update]))

(set! *warn-on-reflection* true)

(declare transact-tx-data)

;; ----------------------------------------------------------------------------


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

(defn ^Boolean seqable?
  [x]
  (and (not (string? x))
    (or (seq? x)
               (instance? clojure.lang.Seqable x)
               (nil? x)
               (instance? Iterable x)
               (arrays/array? x)
               (instance? java.util.Map x))))

(defn combine-hashes [x y]
  (clojure.lang.Util/hashCombine x y))

;; ----------------------------------------------------------------------------

(declare hash-datom)

(declare equiv-datom)

(declare seq-datom)

(declare nth-datom)

(declare assoc-datom)

(declare val-at-datom)

(defprotocol IDatom
  (datom-tx [this])
  (datom-added [this]))

(deftype Datom [e a v tx added ^:unsynchronized-mutable ^int _hash]
  IDatom
  (datom-tx [d] tx)
  (datom-added [d] added)

  Object
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
       (assoc [d k v] (assoc-datom d k v)))


(defn ^Datom datom
  ([e a v] (Datom. e a v tx0 true 0))
  ([e a v tx] (Datom. e a v tx true 0))
  ([e a v tx added] (Datom. e a v tx (boolean added) 0)))

(defn datom? [x] (instance? Datom x))

(defn- hash-datom [^Datom d]
  (-> (hash (.-e d))
    (combine-hashes (hash (.-a d)))
    (combine-hashes (hash (.-v d)))))

(defn- equiv-datom [^Datom d ^Datom o]
  (and (= (.-e d) (.-e o))
    (= (.-a d) (.-a o))
    (= (.-v d) (.-v o))))

(defn- seq-datom [^Datom d]
  (list (.-e d) (.-a d) (.-v d) (datom-tx d) (datom-added d)))

;; keep it fast by duplicating for both keyword and string cases
;; instead of using sets or some other matching func
(defn- val-at-datom [^Datom d k not-found]
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

(defn- nth-datom
  ([^Datom d ^long i]
   (case i
     0 (.-e d)
     1 (.-a d)
     2 (.-v d)
     3 (datom-tx d)
     4 (datom-added d)
     (throw (IndexOutOfBoundsException.))))
  ([^Datom d ^long i not-found]
   (case i
     0 (.-e d)
     1 (.-a d)
     2 (.-v d)
     3 (datom-tx d)
     4 (datom-added d)
     not-found)))

(defn- ^Datom assoc-datom [^Datom d k v]
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

(defmethod print-method Datom [^Datom d, ^java.io.Writer w]
     (.write w (str "#dbval/Datom "))
     (binding [*out* w]
       (pr [(.-e d) (.-a d) (.-v d) (datom-tx d) (datom-added d)])))

;; ----------------------------------------------------------------------------
;; datom cmp macros/funcs
;;

(defn class-identical?
  {:inline (fn [x y] `(identical? (class ~x) (class ~y)))}
  [x y]
  (identical? (class x) (class y)))

(defn class-name
     {:inline
      (fn [x]
        `(let [^Object x# ~x]
           (if (nil? x#) x# (.getName (. x# (getClass))))))}
     ^String [^Object x] (if (nil? x) x (.getName (. x (getClass)))))

(defn class-compare
  ^long [x y]
  (long (compare (class-name x) (class-name y))))

(defmacro int-compare [x y]
  `(long (Integer/compare ~x ~y)))

(defn ihash
  {:inline (fn [x] `(. clojure.lang.Util (hasheq ~x)))}
  ^long [x]
  (. clojure.lang.Util (hasheq x)))

(declare value-compare)

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

(defn value-compare [x y]
  (try
    (cond
      (= x y) 0
      (and (sequential? x) (sequential? y)) (seq-compare x y)
      (instance? Number x)       (clojure.lang.Numbers/compare x y)
      (instance? Comparable x)   (.compareTo ^Comparable x y)
      (not (class-identical? x y)) (class-compare x y)

      :else (int-compare (ihash x) (ihash y)))
    (catch ClassCastException e
      (if (not (class-identical? x y))
        (class-compare x y)
        (throw e)))))

;; ----------------------------------------------------------------------------

(declare db-store db-pending)

(declare indexing?)


(declare resolve-datom)

(declare components->pattern)

(declare resolve-datom*)

(declare components->pattern*)

;;;;;;;;;; Fast validation

(defmacro validate-attr [attr at]
     `(let [attr# ~attr]
        (when-not (or
                    (keyword? attr#)
                    (string? attr#))
          (let [at# ~at]
            (util/raise "Bad entity attribute " attr# " at " at# ", expected keyword or string"
              {:error :transact/syntax, :attribute attr#, :context at#})))))

(defmacro validate-val [v at]
     `(when (nil? ~v)
        (let [at# ~at]
          (util/raise "Cannot store nil as a value at " at#
            {:error :transact/syntax, :value nil, :context at#}))))

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

;; Deref value types: attributes flagged with {:dbval/deref true} keep only a
;; SHA-256 content hash of their value in the index keys; the value's pr-str
;; bytes are stored once, content-addressed, in the store's blob area (see
;; `dbval.store`). Reads return a `BlobRef` and the value is only fetched and
;; parsed on `deref`. This keeps large values (which would otherwise blow up
;; key sizes and scan costs) out of the indexes, while equality — datom
;; equality, transact no-op detection, upserts and datalog joins — keeps
;; working by comparing hashes: same content <=> same hash.

(declare deref-attr? db-get-blob)

(defn sha-256
  "SHA-256 digest of `bytes`."
  ^bytes [^bytes bytes]
  (.digest (java.security.MessageDigest/getInstance "SHA-256") bytes))

(def ^:private blob-unrealized
  "Sentinel marking a BlobRef whose value has not been parsed yet."
  (Object.))

;; `inline-str` is only set on refs deserialized from legacy inline datoms
;; (written before their attribute was flagged as deref): such refs
;; re-serialize to the original inline form, so retraction keys stay adjacent
;; to the assertion keys they cancel out.
(deftype BlobRef [db ^bytes hash ^bytes bytes inline-str
                  ^:unsynchronized-mutable value]
  clojure.lang.IDeref
  (deref [this]
    (locking this
      (when (identical? blob-unrealized value)
        (let [^bytes bs (or bytes (db-get-blob db hash))]
          (when (nil? bs)
            (util/raise "No blob found for deref value"
              {:error :blob/not-found}))
          (set! value (edn/read-string (String. bs java.nio.charset.StandardCharsets/UTF_8)))))
      value))

  clojure.lang.IPending
  (isRealized [this]
    (locking this
      (not (identical? blob-unrealized value))))

  clojure.lang.IHashEq
  (hasheq [this] (java.util.Arrays/hashCode hash))

  Object
  (hashCode [this] (java.util.Arrays/hashCode hash))
  (equals [this other]
    (or (identical? this other)
        (and (instance? BlobRef other)
             (java.util.Arrays/equals hash ^bytes (.-hash ^BlobRef other))))))

(defn- bytes->hex
  ;; java.util.HexFormat needs JDK 17+, but dbval still supports JDK 11
  ^String [^bytes bytes]
  (let [sb (StringBuilder. (* 2 (alength bytes)))]
    (dotimes [i (alength bytes)]
      (let [b (bit-and (aget bytes i) 0xff)]
        (when (< b 0x10)
          (.append sb \0))
        (.append sb (Integer/toHexString b))))
    (str sb)))

(defmethod print-method BlobRef [^BlobRef blob-ref ^java.io.Writer w]
  ;; prints the content hash, never the value: printing (logs, REPL) must not
  ;; fetch the blob
  (.write w "#dbval/blob-ref \"")
  (.write w (bytes->hex (.-hash blob-ref)))
  (.write w "\""))

(defn blob-ref? [x]
  (instance? BlobRef x))

(defn ^BlobRef value->blob-ref
  "Wraps `v` — a value of a deref attribute — into a [[BlobRef]] carrying the
   SHA-256 of its pr-str bytes. Passes an existing BlobRef through unchanged,
   so values copied from query results are never re-serialized or fetched."
  [db v]
  (if (blob-ref? v)
    v
    ;; canonical representative first: pr-str is scale-sensitive, and
    ;; 0.50M must hash to the same blob as 0.5M - the byte encoder gives
    ;; inline attributes exactly that equality. Strict UTF-8: getBytes
    ;; would replace an unpaired surrogate with '?', silently hashing
    ;; distinct values to the same blob.
    (let [v (tuple-codec/canonical-value v)
          ^bytes bs (tuple-codec/utf8-bytes (pr-str v))]
      (BlobRef. db (sha-256 bs) bs nil v))))

;; ----------------------------------------------------------------------------

(defn serialize-tuple
  [attr x]
  (cond
    (or (keyword? x)
        (symbol? x)
        (string? x)
        (instance? java.util.Date x))
    (pr-str x)

    (sequential? x)
    (mapv (fn [item] (serialize-tuple attr item))
          x)

    (tuple-codec/supported-value? x)
    x

    :else
    (util/raise "Value of attribute " attr " contains an element of "
                "unsupported type " (class x) " and cannot be stored inside "
                "index keys. Flag the attribute with {:dbval/deref true} to "
                "store arbitrary edn values in the blob area instead."
      {:error :transact/unsupported-value-type
       :attribute attr
       :value-type (class x)})))

(def max-inline-value-bytes
  "Maximum size of a serialized value inside an index key. Values are stored
   in every index key of their datom, and SlateDB caps keys at 64 KiB, so
   larger values must live in the blob area via a {:dbval/deref true}
   attribute."
  60000)

(defn- validate-inline-size
  ^String [attr ^String s]
  ;; a String of n chars is at least n and at most 3n UTF-8 bytes (surrogate
  ;; pairs of 4-byte code points are 2 chars); only compute the exact byte
  ;; count when the cheap char-count bound cannot rule out an overflow
  (when (and (> (* 3 (.length s)) max-inline-value-bytes)
             (> (alength (.getBytes s java.nio.charset.StandardCharsets/UTF_8))
                max-inline-value-bytes))
    (util/raise "Value of attribute " attr " serializes to more than "
                max-inline-value-bytes " bytes and cannot be stored inside "
                "index keys. Flag the attribute with {:dbval/deref true} to "
                "store its values in the blob area instead."
      {:error :transact/value-too-large
       :attribute attr
       :length (.length s)}))
  s)

(defn- validate-decimal-size
  ;; the encoded decimal is one byte per significant digit plus 7 framing
  ;; bytes (see dbval.tuple/write-decimal); trailing zeros are stripped
  ;; before encoding, so only the stripped precision counts. Every other
  ;; numeric encoding is <= 9 bytes - this is the one number type whose key
  ;; size grows with the value, so it needs the same bound as strings.
  ^java.math.BigDecimal [attr ^java.math.BigDecimal d]
  (let [digits (.precision (.stripTrailingZeros d))]
    (when (> (+ digits 7) max-inline-value-bytes)
      (util/raise "Value of attribute " attr " has " digits " significant "
                  "digits and serializes to more than " max-inline-value-bytes
                  " bytes; it cannot be stored inside index keys. Flag the "
                  "attribute with {:dbval/deref true} to store its values "
                  "in the blob area instead."
        {:error :transact/value-too-large
         :attribute attr
         :digits digits})))
  d)

(defn serialize-value
  [db attr v]
  (cond
    (blob-ref? v)
    (or (.-inline-str ^BlobRef v)
        (.-hash ^BlobRef v))

    ;; nil marks an unbound search-pattern component and must stay nil
    (and (some? v) (deref-attr? db attr))
    (.-hash (value->blob-ref db v))

    (or (map? v)
        (keyword? v)
        (symbol? v)
        (string? v)
        (instance? java.util.Date v))
    (validate-inline-size attr (pr-str v))

    (sequential? v)
    (serialize-tuple attr v)

    (instance? java.math.BigDecimal v)
    (validate-decimal-size attr v)

    (tuple-codec/supported-value? v)
    v

    :else
    (util/raise "Value of attribute " attr " has unsupported type " (class v)
                " and cannot be stored inside index keys. Supported are "
                "strings, keywords, symbols, maps, dates, booleans, UUIDs, "
                "byte arrays, integers, bigints, doubles, floats, bigdecs "
                "and sequential collections of these. Flag the attribute "
                "with {:dbval/deref true} to store arbitrary edn values in "
                "the blob area instead."
      {:error :transact/unsupported-value-type
       :attribute attr
       :value-type (class v)})))

(defn attr-sort-key
  "Returns the serialized key used for attribute components in indexes."
  [attr]
  (when attr
    (pr-str attr)))

(defn compare-attr-keys
  "Compares two serialized attribute keys (see `attr-sort-key`) in
   code-point order — the order of their UTF-8 bytes, which is how the
   store sorts serialized attribute components. Java's String compare
   orders by UTF-16 code units instead, which disagrees for
   supplementary-plane characters (e.g. emoji): surrogates sort below
   U+E000..U+FFFF even though their code points are larger."
  ^long [^String ka ^String kb]
  (cond
    (nil? ka) (if (nil? kb) 0 -1)
    (nil? kb) 1
    :else
    (let [la (.length ka)
          lb (.length kb)]
      (loop [i (int 0)]
        (if (and (< i la) (< i lb))
          (let [ca (.codePointAt ka i)
                cb (.codePointAt kb i)]
            (if (= ca cb)
              (recur (int (+ i (Character/charCount ca))))
              (long (Integer/compare ca cb))))
          (long (Integer/compare la lb)))))))

(defn attr-compare
  "Compares attributes in the same order as dbval indexes store them."
  [a b]
  (compare-attr-keys (attr-sort-key a)
                     (attr-sort-key b)))

(defn tuple-list
  [db order datom]
  (try
    (let [[e a v t added] datom]
      (case (keyword order)
        :eavt
        (list (name order) e (attr-sort-key a) (serialize-value db a v) t added)
        :aevt
        (list (name order) (attr-sort-key a) e (serialize-value db a v) t added)
        :avet
        (list (name order) (attr-sort-key a) (serialize-value db a v) e t added)
        :teav
        (list (name order) t e (attr-sort-key a) (serialize-value db a v) added)
        ))
    (catch Exception e
      (let [err (:error (ex-data e))]
        (if (and (keyword? err) (= "transact" (namespace err)))
          ;; pass validation anomalies through unchanged: wrapping would put
          ;; the offending datom into ex-data and thereby into every log of
          ;; the error
          (throw e)
          (throw (ex-info "tuple-list failed"
                          {:order order
                           :datom datom}
                          e)))))))

(defn tuple-range
  "Returns a vector of the begin and end of the range covering the tuples
   that extend the `components`."
  [& components]
  ;; & rest args are nil when empty; (tuple-range) covers the whole store
  (tuple-codec/range components))

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
  (if (deref-attr? db attr)
    (if (string? v)
      ;; legacy datom written before `attr` was flagged as deref: the
      ;; serialized value still lives inline in the index key. Hashing it
      ;; here keeps equality consistent with blob-backed datoms of the same
      ;; value, since both hash the same pr-str bytes.
      (let [^bytes bs (.getBytes ^String v java.nio.charset.StandardCharsets/UTF_8)]
        (BlobRef. db (sha-256 bs) bs v blob-unrealized))
      (BlobRef. db v nil nil blob-unrealized))
    (if (string? v)
      (edn/read-string v)
      (if (tuple? db attr)
        (deserialize-tuple v)
        (if (-> (-schema db) (get attr) :db/tupleAttrs)
          (deserialize-tuple v)
          v)))))

(defn datom-from-tuple
  "Reads back a datom that was stored as an encoded tuple."
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
  "Converts a byte array back into a tuple (a vector of components)."
  [^bytes bytes]
  (tuple-codec/unpack bytes))

(defn bytes-to-datoms-xf
  [db]
  (comp
   (partial datom-from-tuple
            db)
   tuple-from-bytes))

(defn bytes-to-datoms
  "Converts a collection of byte arrays (encoded tuples) into datoms."
  [db byte-tuples]
  (->Eduction
   (map (bytes-to-datoms-xf db))
   byte-tuples))

(defn pack
  [tuple]
  (try
    (tuple-codec/pack tuple)
    (catch Exception e
      (throw (ex-info "pack failed"
                      {:tuple tuple}
                      e)))))

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

               ;; d2 is a retract unrelated to d1. In a full stream a
               ;; retract directly follows its own add (handled above), but
               ;; a `since` lower bound can orphan a retract by filtering
               ;; out its add — d1 must not be swallowed by it.
               (not (:added d2))
               (do
                 ;; (prn "d2 retract" d1 d2)
                 (vreset! previous d2)
                 (if (:added d1)
                   (rf result d1)
                   result))

               :else
               (do
                 ;; (prn "else" d1 d2)
                 (vreset! previous
                          d2)
                 ;; d1 can be an orphaned retract (see above) — never emit it
                 (if (:added d1)
                   (rf result d1)
                   result))
               ))))))))

(defn datoms-filter-reverse
  "Like `datoms-filter`, but for datoms arriving in descending index order,
   as produced by a reverse `slice`.

   `datoms-filter` relies on ascending order (a datom's `:db/retract`
   directly follows its `:db/add`), so it cannot be applied to a descending
   stream. Datoms that share the same `[e a v]` are still adjacent in a
   descending stream, however, so each such group is buffered, restored to
   ascending order and run through `datoms-filter`, which emits the
   surviving `:db/add` datom, if any."
  [rf]
  (let [buffer      (volatile! [])
        flush-group (fn [result]
                      (let [group @buffer]
                        (vreset! buffer [])
                        (if-some [live (first (into [] datoms-filter (rseq group)))]
                          (rf result live)
                          result)))]
    (fn
      ([] (rf))
      ([result]
       (rf (unreduced (flush-group result))))
      ([result d]
       (let [prev (peek @buffer)]
         (if (or (nil? prev)
                 (and (= (:e prev) (:e d))
                      (= (:a prev) (:a d))
                      (= (:v prev) (:v d))))
           (do
             (vswap! buffer conj d)
             result)
           (let [result' (flush-group result)]
             (when-not (reduced? result')
               (vswap! buffer conj d))
             result')))))))

(defn tx-visibility-xform
  "Composed transducer applying a db value's transaction visibility rules to
   an ascending stream of datoms:

   - upper bound: only datoms with `tx <= max-tx` are visible. This is what
     makes a db an immutable value (and what `as-of` relies on, since
     transaction squuids increase monotonically).
   - optional lower bound: when `since-tx` is set, only datoms with
     `tx > since-tx` are visible (see `since`).
   - `datoms-filter` removes retracted datoms, unless `history?` is set, in
     which case all datom versions (including retractions) are returned
     (see `history`)."
  [max-tx since-tx history?]
  (apply comp
         (concat
          [(filter (fn [datom]
                     (uuid<= (:tx datom)
                             max-tx)))]
          (when since-tx
            [(filter (fn [datom]
                       (pos? (compare (:tx datom) since-tx))))])
          (when-not history?
            [datoms-filter]))))

(defn tx-visibility-xform-reverse
  "Like `tx-visibility-xform`, for a descending stream of datoms (see
   `datoms-filter-reverse`)."
  [max-tx since-tx history?]
  (apply comp
         (concat
          [(filter (fn [datom]
                     (uuid<= (:tx datom)
                             max-tx)))]
          (when since-tx
            [(filter (fn [datom]
                       (pos? (compare (:tx datom) since-tx))))])
          (when-not history?
            [datoms-filter-reverse]))))

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

(defn- merge-sorted
  "Lazily merges two seqs that are sorted by `cmp`, dropping duplicates."
  [cmp xs ys]
  (lazy-seq
    (let [xs (seq xs)
          ys (seq ys)]
      (cond
        (nil? xs) ys
        (nil? ys) xs
        :else
        (let [x (first xs)
              y (first ys)
              c (cmp x y)]
          (cond
            (neg? c) (cons x (merge-sorted cmp (rest xs) ys))
            (pos? c) (cons y (merge-sorted cmp xs (rest ys)))
            :else    (cons x (merge-sorted cmp (rest xs) (rest ys)))))))))

(defn slice
  "Scans the db's store for keys in [begin, end), in unsigned byte order
   (descending when `reverse`). When the db value carries a pending
   transaction overlay, its keys are merged into the scan, so reads during
   a transaction see the transaction's own uncommitted writes."
  [{:keys [db ^bytes begin ^bytes end reverse]}]
  (let [scan (store/scan (db-store db) begin end (boolean reverse))]
    (if-some [^java.util.NavigableSet pending (db-pending db)]
      (let [sub     (.subSet pending begin true end false)
            overlay (if reverse
                      (.descendingSet ^java.util.NavigableSet sub)
                      sub)
            cmp     (if reverse
                      (fn [a b] (store/byte-array-compare b a))
                      (fn [a b] (store/byte-array-compare a b)))]
        (merge-sorted cmp
                      (iterator-seq (.iterator ^java.lang.Iterable scan))
                      (seq overlay)))
      scan)))

;; An opaque handle to a database value, like Datomic's Db: it hashes and
;; compares by reference identity. To compare two snapshots, compare
;; `basis-tx` (and know which store they came from) — content-based value
;; semantics would have to realize a potentially larger-than-memory database.
(deftype DB [schema max-tx rschema pull-patterns pull-attrs
             store pending pending-blobs as-of-tx since-tx history?]


  IDB
  (-schema [db] (.-schema db))
  (-attrs-by [db property] ((.-rschema db) property))

  ISearch
  (-search [db pattern]
    (let [[e a v tx] pattern
          v (if (and (some? a) (some? v) (deref-attr? db a))
              (value->blob-ref db v)
              v)
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
             (tx-visibility-xform max-tx since-tx history?)
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
             (tx-visibility-xform max-tx since-tx history?)
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
             (tx-visibility-xform max-tx since-tx history?))
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
             (tx-visibility-xform-reverse max-tx since-tx history?))
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
             (tx-visibility-xform max-tx since-tx history?))
       (slice {:db db
               :begin begin
               :end end}))))

  clojure.data/EqualityPartition
  (equality-partition [x] :dbval/db)

  ;; Implemented only to throw: without this extension `clojure.data/diff`
  ;; would fall back to its default map implementation and diff the DB
  ;; record's fields (:conn, :max-tx, ...), silently producing
  ;; nonsense.
  clojure.data/Diff
  (diff-similar [a b]
    (throw (UnsupportedOperationException.
             "clojure.data/diff is not supported on dbval databases, since it would realize both databases entirely in memory"))))

(defn db? [x]
  (or
       (and x
         (instance? dbval.db.ISearch x)
         (instance? dbval.db.IIndexAccess x)
         (instance? dbval.db.IDB x))
       (and (satisfies? ISearch x)
         (satisfies? IIndexAccess x)
         (satisfies? IDB x))))

;; ----------------------------------------------------------------------------
;; Like DB, an opaque handle: reference identity only.
(deftype FilteredDB [unfiltered-db pred]


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

(defn basis-tx
  "Returns the transaction id (the basis) up to which this database value
   sees the store. Since db values are opaque handles that compare by
   reference identity, comparing two snapshots of the same store means
   comparing their `basis-tx`."
  [db]
  (.-max-tx (unfiltered-db db)))

(defn ^:no-doc db-store
  "The tuple store backing this database value (see `dbval.store`)."
  [db]
  (.-store (unfiltered-db db)))

(defn- db-pending
  "The pending transaction overlay of this db value: a NavigableSet of the
   keys staged by the transaction that produced it, or nil outside of a
   transaction."
  [db]
  (.-pending (unfiltered-db db)))

(defn- db-pending-blobs
  "The pending blob overlay of this db value: a Map of content hash bytes ->
   value bytes staged by the transaction that produced it, or nil outside of
   a transaction."
  [db]
  (.-pending-blobs (unfiltered-db db)))

(defn ^:no-doc db-get-blob
  "Returns the blob bytes stored under the content `hash` as visible to this
   db value: the pending transaction overlay first, then the committed
   store. Used by BlobRef deref, so a transaction function can deref a value
   asserted earlier in the same transaction."
  ^bytes [db ^bytes hash]
  (or (when-some [^java.util.Map blobs (db-pending-blobs db)]
        (.get blobs hash))
      (store/get-blob (db-store db) hash)))

(defn ^:no-doc ^DB with-max-tx
  "Copy of `db` with a different basis. Low-level; a db value normally gets
   its basis from the store (see `dbval.conn`) or a transaction."
  [^DB db max-tx]
  (DB. (.-schema db) max-tx (.-rschema db)
       (.-pull-patterns db) (.-pull-attrs db)
       (.-store db) (.-pending db) (.-pending-blobs db)
       (.-as-of-tx db) (.-since-tx db) (.-history? db)))

(declare with-pending)

(defn- coerce-tx
  "Coerces `t` — a transaction squuid or an instant — to a transaction id."
  [t]
  (cond
    (uuid? t) t
    (inst? t) (squuid/time->uuid t)
    :else     (util/raise "Expected a transaction UUID or an instant, got " t
                          {:error :time-point/syntax, :t t})))

(defn ^DB as-of
  "Returns the value of the database as of transaction t (a transaction
   squuid or an instant). Since transaction squuids increase strictly
   monotonically, the as-of view is the same database value with its basis
   bounded to t — every index read filters datoms accordingly (see
   `tx-visibility-xform`)."
  [^DB db t]
  {:pre [(instance? DB db)]}
  (let [tx (coerce-tx t)]
    (DB. (.-schema db) tx (.-rschema db)
         (.-pull-patterns db) (.-pull-attrs db)
         (.-store db) (.-pending db) (.-pending-blobs db)
         tx (.-since-tx db) (.-history? db))))

(defn as-of-t
  "Returns the as-of transaction of a database view created by `as-of`, or
   nil if db is not an as-of view."
  [db]
  (.-as-of-tx (unfiltered-db db)))

(defn ^DB since
  "Returns a value of the database containing only datoms asserted after
   transaction t (exclusive). t is a transaction squuid or an instant."
  [^DB db t]
  {:pre [(instance? DB db)]}
  (DB. (.-schema db) (.-max-tx db) (.-rschema db)
       (.-pull-patterns db) (.-pull-attrs db)
       (.-store db) (.-pending db) (.-pending-blobs db)
       (.-as-of-tx db) (coerce-tx t) (.-history? db)))

(defn since-t
  "Returns the since transaction of a database view created by `since`, or
   nil if db is not a since view."
  [db]
  (.-since-tx (unfiltered-db db)))

(defn ^DB history
  "Returns a value of the database containing all datom versions, including
   retractions (`datoms-filter` is skipped, see `tx-visibility-xform`).
   Datoms report assertion vs retraction via their `:added` flag."
  [^DB db]
  {:pre [(instance? DB db)]}
  (DB. (.-schema db) (.-max-tx db) (.-rschema db)
       (.-pull-patterns db) (.-pull-attrs db)
       (.-store db) (.-pending db) (.-pending-blobs db)
       (.-as-of-tx db) (.-since-tx db) true))

(defn temporal-view?
  "True if db is an as-of, since, or history view. Temporal views are
   read-only: they cannot be transacted against."
  [db]
  (let [^DB db (unfiltered-db db)]
    (boolean (or (.-as-of-tx db)
                 (.-since-tx db)
                 (.-history? db)))))

(defn- ^DB with-pending
  "Copy of `db` with different pending key and blob overlays (nil to clear)."
  [^DB db pending pending-blobs]
  (DB. (.-schema db) (.-max-tx db) (.-rschema db)
       (.-pull-patterns db) (.-pull-attrs db)
       (.-store db) pending pending-blobs
       (.-as-of-tx db) (.-since-tx db) (.-history? db)))

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
      (and (= :dbval/deref k) (true? v))    [:dbval/deref]
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

    ;; deref: value lives in the blob area, only its content hash is indexed
    (validate-schema-key a :dbval/deref (:dbval/deref kv) #{true false})
    (when (and (:dbval/deref kv)
               (or (:db/valueType kv) (:db/tupleAttrs kv)))
      (util/raise "Bad attribute specification for " a ": {:dbval/deref true} cannot be combined with :db/valueType or :db/tupleAttrs"
        {:error     :schema/validation
         :attribute a
         :key       :dbval/deref}))

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

(defn- default-store
  "Builds the default SQLite-backed store. Loaded lazily: dbval ships no
   storage driver, so the SQLite adapter (and its driver requirement) is
   only touched when no explicit :store is given."
  [opts]
  ((requiring-resolve 'dbval.store.sqlite/store) opts))

(defn ^DB empty-db [schema opts]
  {:pre [(or (nil? schema) (map? schema))]}
  (validate-schema schema)
  ;; TODO: consider how to close the store:
  (let [store (or (:store opts)
                  (default-store opts))
        db    (DB. schema
                   tx0
                   (rschema (merge implicit-schema schema))
                   (lru/cache 100)
                   (lru/cache 100)
                   store
                   nil nil nil nil nil)]
    (with-max-tx db (q-max-tx db))))

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

(defn ^DB with-schema [^DB db schema]
  {:pre [(db? db) (or (nil? schema) (map? schema))]}
  (DB. schema
       (.-max-tx db)
       (rschema (merge implicit-schema schema))
       (lru/cache 100)
       (lru/cache 100)
       (.-store db)
       (.-pending db)
       (.-pending-blobs db)
       (.-as-of-tx db)
       (.-since-tx db)
       (.-history? db)))


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
     (defmethod print-method FilteredDB [db w] (pr-db db w)))

(defn db-from-reader [{:keys [schema datoms]}]
  (init-db (map (fn [[e a v tx]] (datom e a v tx)) datoms) schema {}))

;; ----------------------------------------------------------------------------

(declare entid-strict)

(declare ref?)

(defn resolve-pattern-v
  "Resolves the value component of a search pattern: entity ids for ref
   attributes, BlobRefs for deref attributes (so both the scan range and the
   `datom=` post-filter compare content hashes)."
  [db a v]
  (cond
    (not (some? v))    v
    (ref? db a)        (entid-strict db v)
    (deref-attr? db a) (value->blob-ref db v)
    :else              v))

(defn resolve-datom [db e a v t default-e default-tx]
  (when (some? a)
    (validate-attr a (list 'resolve-datom 'db e a v t)))
  (datom
    (if (some? e) (entid-strict db e) default-e)
    a
    (resolve-pattern-v db a v)
    (if (some? t) (entid-strict db t) default-tx)))

(defn components->pattern [db index c0 c1 c2 c3 default-e default-tx]
  (case index
    :eavt (resolve-datom db c0 c1 c2 c3 default-e default-tx)
    :aevt (resolve-datom db c1 c0 c2 c3 default-e default-tx)
    :avet (resolve-datom db c2 c0 c1 c3 default-e default-tx)))

(defn resolve-datom* [db e a v t]
  (when (some? a)
    (validate-attr a (list 'resolve-datom 'db e a v t)))
  [(when (some? e) (entid-strict db e))
   a
   (resolve-pattern-v db a v)
   (when (some? t) (entid-strict db t))])

(defn components->pattern* [db index c0 c1 c2 c3]
  (case index
    :eavt (resolve-datom* db c0 c1 c2 c3)
    :aevt (resolve-datom* db c1 c0 c2 c3)
    :avet (resolve-datom* db c2 c0 c1 c3)))

(defn find-datom [db index c0 c1 c2 c3]
  (validate-indexed db index c0 c1 c2 c3)
  (first (-datoms db index c0 c1 c2 c3)))

;; ----------------------------------------------------------------------------

(defn is-attr? [db attr property]
  (contains? (-attrs-by db property) attr))

(defn multival? [db attr]
  (is-attr? db attr :db.cardinality/many))

(defn multi-value? [db attr value]
  (and
    (is-attr? db attr :db.cardinality/many)
    (or
      (arrays/array? value)
      (and (coll? value) (not (map? value))))))

(defn ref? [db attr]
  (is-attr? db attr :db.type/ref))

(defn component? [db attr]
  (is-attr? db attr :db/isComponent))

(defn indexing? [db attr]
  (is-attr? db attr :db/index))

(defn tuple? [db attr]
  (is-attr? db attr :db.type/tuple))

(defn deref-attr?
  "True if `attr` is flagged with {:dbval/deref true}: its values are stored
   content-addressed in the blob area and surface as BlobRefs."
  [db attr]
  (is-attr? db attr :dbval/deref))

(defn tuple-source? [db attr]
  (is-attr? db attr :db/attrTuples))

(defn reverse-ref? [attr]
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
          (let [tuple-attrs (-> (-schema db) (get tuple) :db/tupleAttrs)
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

(defn entid [db eid]
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


    (keyword? eid)
    (-> (-datoms db :avet :db/ident eid nil nil) first :e)

    :else
    (util/raise "Expected UUID or lookup ref for entity id, got " eid
      {:error :entity-id/syntax, :entity-id eid})))

(defn eid-exists? [db eid]
  (= eid (-> (-seek-datoms db :eavt eid nil nil nil) first :e)))

(defn entid-strict [db eid]
  (or
    (entid db eid)
    (util/raise "Nothing found for entity id " eid
      {:error :entity-id/missing
       :entity-id eid})))

(defn entid-some [db eid]
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

(defn- new-eid
  "Generates a new entity id. Uses sequential uuids (squuids) so that entity
   ids sort by creation time, similar to Datomic's monotonically increasing
   numeric eids. Applications rely on this for newest-first ordering by eid."
  []
  (squuid/generate-squuid))

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
                                  (let [new-uuid (new-eid)]
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
                                  (let [new-uuid (new-eid)]
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
                    (let [uuid (new-eid)]
                      (swap! id-map assoc id uuid)
                      uuid)))
                :else id))
            (process-entity [entity]
              (util/cond+
                (map? entity)
                (let [old-id (:db/id entity)
                      new-id (if (contains? entity :db/id)
                               (resolve-id old-id)
                               (new-eid))]
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
  "Generates a new sequential UUID (squuid) for an entity."
  [_db]
  (new-eid))

(defn- ^Boolean tx-id?
     [e]
     (or (identical? :db/current-tx e)
       (.equals ":db/current-tx" e) ;; for dbval.js interop
       (.equals "datomic.tx" e)
       (.equals "dbval.tx" e)))

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
  [db ^java.util.NavigableSet pending tuple]
  (try
    (.add pending (pack tuple))
    (catch Exception e
      (throw (ex-info "set-add! failed"
                      {:tuple tuple}
                      e))))
  db)

(defn all-tuples
  "Returns a reducible of all tuples in the db's store (committed, plus the
   pending overlay if this db value carries one). Each tuple is decoded from
   its byte representation. Useful for debugging and inspecting the raw
   storage. Example: (into [] (take 10) (all-tuples db))"
  [db]
  (let [[begin end] (tuple-range)]
    (eduction (map (comp vec tuple-from-bytes))
              (slice {:db db :begin begin :end end}))))

(defn- find-exact-datom
  "Finds the current datom with exactly [e a v]. For deref attributes this
   also finds legacy inline datoms (written before the attribute was flagged
   as deref), which the hash-ranged search cannot see."
  ^Datom [db e a v]
  (or (fsearch db [e a v])
      (when (deref-attr? db a)
        (some (fn [^Datom d] (when (= (.-v d) v) d))
              (-search db [e a])))))

(defn- stage-blob!
  "Stages the blob behind `blob-ref` into the transaction's pending blob
   overlay, so it is committed atomically with its datom's keys."
  [db ^BlobRef blob-ref]
  (let [^java.util.Map blobs (db-pending-blobs db)
        ^bytes hash (.-hash blob-ref)]
    (when (nil? blobs)
      (util/raise "stage-blob! outside of a transaction"
        {:error :transact/no-pending}))
    (when-not (.containsKey blobs hash)
      (if-some [^bytes bs (.-bytes blob-ref)]
        (.put blobs hash bs)
        ;; a BlobRef without bytes came from a query, so its blob normally
        ;; already lives in this store; fetch it from the ref's origin db
        ;; only when it does not (a value copied from another database)
        (when (nil? (store/get-blob (db-store db) hash))
          (if-some [^bytes bs (db-get-blob (.-db blob-ref) hash)]
            (.put blobs hash bs)
            (util/raise "No blob found for deref value"
              {:error :blob/not-found})))))))

(defn with-datom [db ^Datom datom]
  (validate-datom db datom)
  (let [^java.util.NavigableSet pending (db-pending db)
        _ (when (nil? pending)
            (util/raise "with-datom outside of a transaction"
              {:error :transact/no-pending}))
        indexing? (indexing? db (.-a datom))]
    (if (datom-added datom)
      (do
        ;; legacy refs (inline-str) serialize back into the key itself and
        ;; need no blob
        (when (and (blob-ref? (.-v datom))
                   (nil? (.-inline-str ^BlobRef (.-v datom))))
          (stage-blob! db (.-v datom)))
        (-> db
            (set-add! pending (tuple-list db :eavt datom))
            (set-add! pending (tuple-list db :aevt datom))
            (cond-> indexing? (set-add! pending (tuple-list db :avet datom)))
            (set-add! pending (tuple-list db :teav datom))))
      (if-some [removing (some-> (find-exact-datom db (.-e datom) (.-a datom) (.-v datom))
                                 (retract-datom (:tx datom)))]
        (-> db
            (set-add! pending (tuple-list db :eavt removing))
            (set-add! pending (tuple-list db :aevt removing))
            (cond-> indexing? (set-add! pending (tuple-list db :avet removing)))
            (set-add! pending (tuple-list db :teav removing)))
        db))))

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
        v         (cond
                    (ref? db a)        (entid-strict db v)
                    (deref-attr? db a) (value->blob-ref db v)
                    ;; the datom carries the canonical representative
                    ;; (0.5M, never 0.50M), so tx-data reports the value
                    ;; every later read returns
                    :else              (tuple-codec/canonical-value v))
        new-datom (datom e a v tx)
        multival? (multival? db a)
        old-datom ^Datom (if multival?
                           (find-exact-datom db e a v)
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

(declare transact-tx-data-impl)

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

(defn transact-tx-data-impl [initial-report initial-es]
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
          (update :db-after with-max-tx (current-tx report))
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
                  ov     (cond
                           (ref? db a)        (entid-strict db ov)
                           (and (some? ov)
                                (deref-attr? db a)) (value->blob-ref db ov)
                           :else              ov)
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
            (let [tuple-attrs   (-> (-schema db) (get a) :db/tupleAttrs)
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
              (let [v (cond
                        (ref? db a)        (entid-strict db v)
                        (deref-attr? db a) (value->blob-ref db v)
                        :else              v)]
                (validate-attr a entity)
                (validate-val v entity)
                (if-some [old-datom (find-exact-datom db e a v)]
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

(defn- next-tx-id
  [db]
  (let [current-basis (basis-tx db)
        generated-tx (squuid/generate-squuid)]
    (if (pos? (compare generated-tx current-basis))
      generated-tx
      (squuid-uuid/inc-uuid current-basis))))

(defn transact-tx-data [report es]
  (when-not (or
              (nil? es)
              (sequential? es))
    (util/raise "Bad transaction data " es ", expected sequential collection"
      {:error :transact/syntax, :tx-data es}))
  (when (temporal-view? (:db-before report))
    (util/raise "Cannot transact against an as-of/since/history database value"
      {:error :transact/temporal-view}))
  (let [dry-run? (::dry-run report)
        tx-id   (next-tx-id (:db-after report))
        ;; The pending overlay collects this transaction's keys; reads during
        ;; the transaction merge it over the store (see `slice`), so nothing
        ;; touches the store until the final atomic commit — an exception
        ;; while transacting simply discards the overlay.
        pending (java.util.TreeSet. ^java.util.Comparator store/byte-array-comparator)
        ;; The blobs of deref values staged by this transaction (content hash
        ;; -> value bytes); committed atomically with the pending keys and
        ;; overlaid over the store's blob reads in the meantime (see
        ;; `db-get-blob`).
        pending-blobs (java.util.TreeMap. ^java.util.Comparator store/byte-array-comparator)
        ;; Carry over speculative datoms when chaining a dry-run on a dry-run
        ;; db-after, so they stay visible in the new speculative view. A real
        ;; transact must not inherit them: they were never committed and would
        ;; go missing from storage.
        _ (when-some [^java.util.NavigableSet prev (db-pending (:db-after report))]
            (if dry-run?
              (.addAll ^java.util.TreeSet pending prev)
              (util/raise "Cannot transact against a speculative (dry-run) database value"
                {:error :transact/speculative-view})))
        _ (when dry-run?
            (when-some [^java.util.Map prev (db-pending-blobs (:db-after report))]
              (.putAll pending-blobs prev)))
        report' (-> report
                    (assoc ::tx-id tx-id)
                    ;; Set max-tx to current tx-id so datoms added during this
                    ;; transaction are visible when searching for duplicates
                    (update :db-after with-max-tx tx-id)
                    (update :db-after with-pending pending pending-blobs))
        {:keys [tx-data id-map]} (assign-entity-ids (:db-before report') es)
        ;; Pre-populate tempids with the tempid -> UUID mapping
        report'' (update report' :tempids merge id-map)
        result   (transact-tx-data-impl report'' tx-data)]
    (if dry-run?
      ;; Speculative transaction: nothing is written to the store. Keep the
      ;; pending overlay on db-after so reads see the new datoms via `slice`.
      (-> result
          (dissoc ::dry-run)
          (assoc :tx tx-id))
      (do
        (store/commit! (db-store (:db-after result)) (seq pending) pending-blobs)
        (-> result
            (update :db-after with-pending nil nil)
            ;; Add :tx field with the transaction UUID
            (assoc :tx tx-id))))))
