(ns dbval.test.tuple
  "Tests for the tuple byte encoding.

   The encoding must stay byte-compatible with the FoundationDB tuple layer
   for every type that layer supports, so existing stores remain readable.
   fdb-java is a :dev-only dependency that serves as the differential-testing
   oracle here; the library itself no longer depends on it.

   BigDecimal is dbval's own extension (type code 0x23), so it has no oracle;
   its ordering property is checked generatively instead."
  (:require
    [clojure.test :as t :refer [is deftest testing]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [dbval.store :as store]
    [dbval.tuple :as tuple]))

(defn- fdb-pack
  "Reference encoding via the FoundationDB tuple layer."
  ^bytes [components]
  (.pack (.addAll (com.apple.foundationdb.tuple.Tuple.)
                  ^java.util.List (mapv (fn [x]
                                          (if (sequential? x)
                                            (.addAll (com.apple.foundationdb.tuple.Tuple.)
                                                     ^java.util.List (vec x))
                                            x))
                                        components))))

(defn- fdb-range
  [components]
  (let [r (.range ^com.apple.foundationdb.tuple.Tuple
                  (.addAll (com.apple.foundationdb.tuple.Tuple.)
                           ^java.util.List (vec components)))]
    [(.begin r) (.end r)]))

(def ^:private gen-fdb-scalar
  "Scalars the FoundationDB tuple layer supports (no BigDecimal)."
  (gen/one-of
    [(gen/return nil)
     gen/boolean
     gen/large-integer
     (gen/fmap #(.multiply (java.math.BigInteger/valueOf %)
                           (java.math.BigInteger/valueOf Long/MAX_VALUE))
               gen/large-integer)
     (gen/double* {:NaN? false})
     (gen/fmap float (gen/double* {:NaN? false
                                   :min -3.0e38
                                   :max 3.0e38}))
     gen/string
     gen/bytes
     gen/uuid]))

(def ^:private gen-fdb-tuple
  (gen/vector (gen/one-of [gen-fdb-scalar
                           (gen/vector gen-fdb-scalar 0 3)])
              0 6))

(def ^:private gen-bigdec
  (gen/fmap (fn [[unscaled scale]]
              (java.math.BigDecimal. (java.math.BigInteger/valueOf unscaled)
                                     (int scale)))
            (gen/tuple gen/large-integer (gen/choose -25 25))))

(defn- normalize
  "Maps a value to the representation `unpack` returns, for comparing
   round-trips: byte arrays as seqs, nested lists as vectors, integral
   types narrowed like the decoder narrows them."
  [x]
  (cond
    (bytes? x) (seq x)
    (sequential? x) (mapv normalize x)
    (instance? java.math.BigInteger x) (if (<= (.bitLength ^java.math.BigInteger x) 63)
                                         (.longValueExact ^java.math.BigInteger x)
                                         x)
    (int? x) (long x)
    :else x))

(defspec pack-is-byte-compatible-with-foundationdb 500
  (prop/for-all [components gen-fdb-tuple]
    (java.util.Arrays/equals (tuple/pack components)
                             (fdb-pack components))))

(defspec pack-unpack-round-trips 500
  (prop/for-all [components gen-fdb-tuple]
    (= (normalize components)
       (normalize (tuple/unpack (tuple/pack components))))))

(defspec range-is-byte-compatible-with-foundationdb 100
  (prop/for-all [components gen-fdb-tuple]
    (let [[begin end] (tuple/range components)
          [fdb-begin fdb-end] (fdb-range components)]
      (and (java.util.Arrays/equals ^bytes begin ^bytes fdb-begin)
           (java.util.Arrays/equals ^bytes end ^bytes fdb-end)))))

(defspec bigdec-order-matches-byte-order 500
  (prop/for-all [a gen-bigdec
                 b gen-bigdec]
    (let [byte-order (store/byte-array-compare (tuple/pack [a])
                                               (tuple/pack [b]))
          value-order (.compareTo ^java.math.BigDecimal a b)]
      (= (Long/signum byte-order)
         (Long/signum (long value-order))))))

(defspec bigdec-round-trips-numerically-equal 500
  (prop/for-all [d gen-bigdec]
    (let [[decoded] (tuple/unpack (tuple/pack [d]))]
      (zero? (.compareTo ^java.math.BigDecimal d decoded)))))

(deftest test-bigdec-canonicalization
  ;; numerically equal BigDecimals must encode identically, so index
  ;; lookups and retractions match across scale representations
  (is (java.util.Arrays/equals (tuple/pack [0.5M])
                               (tuple/pack [0.50M])))
  (is (java.util.Arrays/equals (tuple/pack [500M])
                               (tuple/pack [5E+2M])))
  (is (= [0.5M] (tuple/unpack (tuple/pack [0.50M]))))
  (is (= [500M] (tuple/unpack (tuple/pack [5E+2M]))))
  (is (= [0M] (tuple/unpack (tuple/pack [0.000M])))))

(deftest test-bigdec-examples
  ;; the motivating regression: 0.5M went through FoundationDB's
  ;; Number.longValue() fallback and was stored as 0
  (is (= [0.5M] (tuple/unpack (tuple/pack [0.5M]))))
  (doseq [[smaller larger] [[0.49M 0.5M]
                            [0.5M 0.55M]
                            [-0.55M -0.5M]
                            [-0.5M 0M]
                            [0M 0.05M]
                            [9.99M 10M]
                            [-10M -9.99M]
                            [99M 100M]]]
    (is (neg? (store/byte-array-compare (tuple/pack [smaller])
                                        (tuple/pack [larger])))
        (str smaller " should sort before " larger))))

(deftest test-bigdec-in-nested-tuple
  (is (= [["a" 0.5M] 1]
         (tuple/unpack (tuple/pack [["a" 0.5M] 1])))))

(deftest test-unsupported-types-throw
  ;; reject unknown Number subclasses instead of silently truncating
  ;; like FoundationDB's Number.longValue() fallback did
  (is (thrown? clojure.lang.ExceptionInfo (tuple/pack [1/3])))
  (is (thrown? clojure.lang.ExceptionInfo
        (tuple/pack [(java.util.concurrent.atomic.AtomicLong. 5)])))
  (is (thrown? clojure.lang.ExceptionInfo (tuple/pack [\a]))))

(deftest test-long-boundaries
  (doseq [n [0 1 -1 255 256 -255 -256 65535 65536 -65536
             Long/MAX_VALUE Long/MIN_VALUE
             (inc Long/MIN_VALUE) (dec Long/MAX_VALUE)
             ;; 8-byte magnitudes beyond the long range still use the
             ;; plain int type codes; 9+ bytes use the bignum codes
             (biginteger (inc' Long/MAX_VALUE))
             (biginteger (dec' Long/MIN_VALUE))
             (biginteger (-' (*' 2 (inc' Long/MAX_VALUE)) 2))
             (biginteger (-' 2 (*' 2 (inc' Long/MAX_VALUE))))
             (biginteger (*' 2 (inc' Long/MAX_VALUE)))
             (biginteger (-' (*' 2 (inc' Long/MAX_VALUE))))
             (biginteger (*' Long/MAX_VALUE Long/MAX_VALUE))
             (biginteger (-' (*' Long/MAX_VALUE Long/MAX_VALUE)))]]
    (is (java.util.Arrays/equals (tuple/pack [n]) (fdb-pack [n]))
        (str "byte compatibility for " n))
    (is (= [n] (tuple/unpack (tuple/pack [n])))
        (str "round trip for " n))))
