(ns dbval.tuple
  "Order-preserving tuple encoding for datoms.

   Encodes a tuple (a vector of components) into a byte array whose unsigned
   lexicographic byte order equals the semantic order of the tuple - the
   property every dbval index relies on for range scans.

   The format is byte-compatible with the FoundationDB tuple layer
   (https://github.com/apple/foundationdb/blob/main/design/tuple.md) for
   every type dbval ever wrote through that layer: nil, byte arrays,
   strings, nested tuples, integers (including arbitrary precision),
   floats, doubles, booleans and UUIDs. Byte compatibility means existing
   stores written via com.apple.foundationdb.tuple.Tuple stay readable, and
   the implementation can be differential-tested against the FoundationDB
   one. (Versionstamps, the one FoundationDB tuple type dbval never used,
   are not supported.)

   On top of that the format adds one type code of its own:

   - 0x40 BigDecimal, encoded order-preservingly (see `write-decimal`).
     0x40 comes from the 0x40-0x4F range the FoundationDB spec reserves for
     third-party extensions. (0x23/0x24 are reserved by that spec for a
     future arbitrary-precision decimal with a different, non-order-
     preserving encoding, so they must not be reused.) FoundationDB never
     shipped a decimal type because its tuples must be readable from every
     binding language; these tuples are only ever read by Clojure on the
     JVM, so that constraint does not apply here.

   Values of unsupported types are rejected with an exception. This matters:
   the FoundationDB Java implementation falls back to Number.longValue() for
   unknown Number subclasses, which silently truncated BigDecimals
   (0.5M was stored as 0).

   Stores written by the FoundationDB-backed code may still contain such
   truncated datoms: BigDecimals - and clojure.lang.BigInts beyond the long
   range - were stored under long-typed keys. They decode as those longs
   and cannot be matched or retracted by the original value. When upgrading
   such a store, scan attributes that ever held big decimals or bigints for
   long-typed values and re-assert them."
  (:refer-clojure :exclude [range]))

(set! *warn-on-reflection* true)

(def ^:private ^:const type-null 0x00)
(def ^:private ^:const type-bytes 0x01)
(def ^:private ^:const type-string 0x02)
(def ^:private ^:const type-nested 0x05)
(def ^:private ^:const type-neg-bignum 0x0b)
(def ^:private ^:const type-int-zero 0x14)
(def ^:private ^:const type-pos-bignum 0x1d)
(def ^:private ^:const type-float 0x20)
(def ^:private ^:const type-double 0x21)
;; 0x40 is from the user-extension range of the FoundationDB tuple spec;
;; 0x23/0x24 are reserved there for an incompatible decimal encoding
(def ^:private ^:const type-decimal 0x40)
(def ^:private ^:const type-false 0x26)
(def ^:private ^:const type-true 0x27)
(def ^:private ^:const type-uuid 0x30)

;; decimal sign markers: negative < zero < positive in unsigned byte order
(def ^:private ^:const decimal-negative 0x7e)
(def ^:private ^:const decimal-zero 0x80)
(def ^:private ^:const decimal-positive 0x82)

;; ---------------------------------------------------------------------------
;; encoding

(definterface IByteBuf
  (^void writeByte [^long b])
  (^void writeBytes [^bytes bs ^long off ^long n])
  (^bytes toBytes []))

;; unsynchronized replacement for ByteArrayOutputStream: pack runs four
;; times per datom on every transact plus for every range bound, and
;; BAOS.write takes a monitor per byte
(deftype ByteBuf [^:unsynchronized-mutable ^bytes buf
                  ^:unsynchronized-mutable ^long len]
  IByteBuf
  (writeByte [_ b]
    (when (= len (alength buf))
      (set! buf (java.util.Arrays/copyOf buf (* 2 (alength buf)))))
    (aset buf len (unchecked-byte b))
    (set! len (inc len)))
  (writeBytes [_ bs off n]
    (let [need (+ len n)]
      (when (< (alength buf) need)
        (loop [cap (* 2 (alength buf))]
          (if (< cap need)
            (recur (* 2 cap))
            (set! buf (java.util.Arrays/copyOf buf cap))))))
    (System/arraycopy bs off buf len n)
    (set! len (+ len n)))
  (toBytes [_]
    (java.util.Arrays/copyOf buf len)))

(defn- write-escaped
  "Writes `bs` with 0x00 escaped as 0x00 0xFF, then a 0x00 terminator.
   The escaping keeps the bytes self-delimiting while preserving order.
   Bulk-copies the chunks between NULs; the common case (no NUL at all)
   is a single copy."
  [^IByteBuf out ^bytes bs]
  (let [n (alength bs)]
    (loop [start 0
           i 0]
      (if (< i n)
        (if (zero? (aget bs i))
          (do ;; include the 0x00 itself in the chunk, then its 0xFF escape
              (.writeBytes out bs start (- (inc i) start))
              (.writeByte out 0xff)
              (recur (inc i) (inc i)))
          (recur start (inc i)))
        (.writeBytes out bs start (- n start)))))
  (.writeByte out 0x00))

(defn- write-be
  "Writes the lowest `len` bytes of `n` big-endian."
  [^IByteBuf out ^long n ^long len]
  (loop [shift (* 8 (dec len))]
    (when (<= 0 shift)
      (.writeByte out (int (bit-and 0xff (unsigned-bit-shift-right n shift))))
      (recur (- shift 8)))))

(defn- magnitude-length
  "Number of bytes needed for the magnitude of long `n` (n != 0)."
  ^long [^long n]
  (if (= n Long/MIN_VALUE)
    8
    (let [bits (- 64 (Long/numberOfLeadingZeros (Math/abs n)))]
      (quot (+ bits 7) 8))))

(defn- write-long-value
  [^IByteBuf out ^long n]
  (cond
    (zero? n)
    (.writeByte out type-int-zero)

    (pos? n)
    (let [len (magnitude-length n)]
      (.writeByte out (int (+ type-int-zero len)))
      (write-be out n len))

    :else
    (let [len (magnitude-length n)]
      (.writeByte out (int (- type-int-zero len)))
      ;; offset encoding: n + 2^(8*len) - 1; for len 8 the addition of 2^64
      ;; is a no-op in 64-bit two's complement, so it reduces to n - 1
      (write-be out
                (if (= len 8)
                  (unchecked-dec n)
                  (+ n (dec (bit-shift-left 1 (* 8 len)))))
                len))))

(defn- bigint-magnitude-bytes
  "Big-endian magnitude of a non-negative BigInteger, without the leading
   zero byte BigInteger.toByteArray adds to keep the sign bit clear."
  ^bytes [^java.math.BigInteger magnitude]
  (let [^bytes bs (.toByteArray magnitude)]
    (if (and (< 1 (alength bs))
             (zero? (aget bs 0)))
      (java.util.Arrays/copyOfRange bs 1 (alength bs))
      bs)))

(defn- write-biginteger
  "Magnitudes of up to 8 bytes use the plain int type codes (also beyond
   the long range - the type code encodes the magnitude length, not the
   value range); only longer magnitudes use the bignum type codes with an
   explicit length byte."
  [^IByteBuf out ^java.math.BigInteger n]
  (if (<= (.bitLength n) 63)
    (write-long-value out (.longValueExact n))
    (let [^bytes magnitude (bigint-magnitude-bytes (.abs n))
          len (alength magnitude)]
      (when (< 0xff len)
        (throw (ex-info "BigInteger magnitude exceeds 255 bytes"
                        {:value n})))
      (if (pos? (.signum n))
        (do (if (<= len 8)
              (.writeByte out (int (+ type-int-zero len)))
              (do (.writeByte out type-pos-bignum)
                  (.writeByte out (int len))))
            (.writeBytes out magnitude 0 len))
        ;; offset encoding like small negative ints: n + 2^(8*len) - 1
        (let [offset (.subtract (.shiftLeft java.math.BigInteger/ONE (int (* 8 len)))
                                java.math.BigInteger/ONE)
              ^bytes adjusted (bigint-magnitude-bytes (.add n offset))]
          (if (<= len 8)
            (.writeByte out (int (- type-int-zero len)))
            (do (.writeByte out type-neg-bignum)
                (.writeByte out (int (bit-xor len 0xff)))))
          ;; the adjusted value may need fewer bytes than the magnitude;
          ;; left-pad with zeros to keep the width the type code claims
          (dotimes [_ (- len (alength adjusted))]
            (.writeByte out 0x00))
          (.writeBytes out adjusted 0 (alength adjusted)))))))

(defn- write-double-value
  ;; raw bits, like fdb-java: doubleToLongBits would canonicalize NaN
  ;; payloads, so a decoded legacy NaN would re-encode to different bytes
  ;; than its stored key and e.g. never cancel on retraction
  [^IByteBuf out ^double d]
  (let [bits (Double/doubleToRawLongBits d)
        ;; sign bit set: flip all bits; else: flip only the sign bit -
        ;; makes the IEEE bytes sort in numeric order
        bits (if (neg? bits)
               (bit-not bits)
               (bit-xor bits Long/MIN_VALUE))]
    (.writeByte out type-double)
    (write-be out bits 8)))

(defn- write-float-value
  [^IByteBuf out ^Float f]
  (let [bits (Float/floatToRawIntBits (float f))
        bits (if (neg? bits)
               (bit-not bits)
               (bit-xor bits Integer/MIN_VALUE))]
    (.writeByte out type-float)
    (write-be out (bit-and bits 0xffffffff) 4)))

(defn- canonical-decimal
  "Strips trailing zeros, so numerically equal BigDecimals (0.5M vs 0.50M,
   500M vs 5E+2M) encode identically. Never expands the unscaled value:
   1E+100000M stays (unscaled 1, scale -100000) instead of materializing
   100,000 zero digits into the encoding."
  ^java.math.BigDecimal [^java.math.BigDecimal d]
  (.stripTrailingZeros d))

;; decoded integral values are returned at scale 0 (500 instead of 5E+2),
;; but only up to this many restored trailing zeros - a huge exponent
;; (1E+100000M) must never materialize its zero digits on read
(def ^:private ^:const max-restored-trailing-zeros 100)

(defn- restore-plain-integer
  "Returns integral `d` at scale 0 when that adds at most
   `max-restored-trailing-zeros` zero digits; otherwise returns `d`
   unchanged. Purely cosmetic: `canonical-decimal` strips trailing zeros
   before encoding, so both representations produce the same bytes."
  ^java.math.BigDecimal [^java.math.BigDecimal d]
  (if (and (neg? (.scale d))
           (<= (- (.scale d)) max-restored-trailing-zeros))
    (.setScale d 0)
    d))

(defn canonical-value
  "Returns the representative of `x` that decoding its encoded bytes
   returns: BigDecimals are stripped of trailing zeros (integral values
   restored to scale 0 within [[restore-plain-integer]]'s bound), every
   other type passes through unchanged. Datoms and blob content hashes must
   carry this representative so that index bytes, deref hashes, tx-data and
   reads all agree on one value - 0.50M and 0.5M are the same number and
   must behave as the same value everywhere."
  [x]
  (if (instance? java.math.BigDecimal x)
    (restore-plain-integer (.stripTrailingZeros ^java.math.BigDecimal x))
    x))

(defn- write-decimal
  "Order-preserving BigDecimal encoding:

     zero:     0x80
     positive: 0x82, adjusted exponent as offset-binary int32 big-endian,
               mantissa digits (one digit per byte, digit + 1), 0x00
     negative: 0x7E, then the positive encoding of the absolute value with
               every byte XOR 0xFF

   A nonzero decimal is normalized to 0.d1..dn * 10^E with d1 != 0 and
   dn != 0; E is the adjusted exponent. Comparing the exponent first and the
   digits lexicographically then equals numeric comparison; the 0x00
   terminator sorts a digit-prefix (fewer digits, e.g. 0.5 vs 0.55) first,
   which is numerically correct. XOR-ing the negative encoding reverses the
   order for negative values.

   The adjusted exponent must fit an int32 (only violated near BigDecimal's
   scale limits, e.g. 1E+2147483647M); beyond that the offset encoding would
   wrap and mis-sort, so such values are rejected."
  [^IByteBuf out ^java.math.BigDecimal d]
  (.writeByte out type-decimal)
  (let [d (canonical-decimal d)]
    (if (zero? (.signum d))
      (.writeByte out decimal-zero)
      (let [abs (.abs d)
            digits (str (.unscaledValue abs))
            exponent (- (.precision abs) (.scale abs))
            _ (when-not (<= Integer/MIN_VALUE exponent Integer/MAX_VALUE)
                (throw (ex-info "BigDecimal adjusted exponent exceeds the int32 range of the encoding"
                                {:value d
                                 :adjusted-exponent exponent})))
            negative? (neg? (.signum d))
            mask (if negative? 0xff 0x00)]
        (.writeByte out (int (if negative?
                           decimal-negative
                           decimal-positive)))
        (write-be out
                  (bit-xor (bit-and (+ exponent 0x80000000) 0xffffffff)
                           (* mask 0x01010101))
                  4)
        (dotimes [i (.length digits)]
          (.writeByte out (int (bit-xor (inc (- (int (.charAt digits i)) (int \0)))
                                    mask))))
        (.writeByte out (int (bit-xor 0x00 mask)))))))

(defn utf8-bytes
  "Encodes `s` as UTF-8, rejecting unpaired surrogates instead of silently
   replacing them with '?' like String.getBytes does. Distinct strings must
   never alias to the same bytes: under replacement, \"a\\ud800b\" and
   \"a?b\" would encode (and content-hash) identically, so lookups and
   retractions would hit the wrong datom. A typical source of unpaired
   surrogates is a string truncated in the middle of an emoji by `subs`."
  ^bytes [^String s]
  (let [n (.length s)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt s i)]
          (cond
            (and (Character/isHighSurrogate c)
                 (< (inc i) n)
                 (Character/isLowSurrogate (.charAt s (inc i))))
            (recur (+ i 2))

            (Character/isSurrogate c)
            (throw (ex-info "String contains an unpaired surrogate and has no UTF-8 encoding"
                            {:char-index i
                             :char (int c)}))

            :else
            (recur (inc i))))))
    (.getBytes s java.nio.charset.StandardCharsets/UTF_8)))

(defn- write-value
  [^IByteBuf out x nested?]
  (cond
    (nil? x)
    (do (.writeByte out type-null)
        (when nested?
          (.writeByte out 0xff)))

    (string? x)
    (do (.writeByte out type-string)
        (write-escaped out (utf8-bytes x)))

    (bytes? x)
    (do (.writeByte out type-bytes)
        (write-escaped out x))

    (instance? Boolean x)
    (.writeByte out (int (if x type-true type-false)))

    (instance? Long x) (write-long-value out x)
    (instance? Integer x) (write-long-value out (long x))
    (instance? Short x) (write-long-value out (long x))
    (instance? Byte x) (write-long-value out (long x))
    (instance? java.math.BigInteger x) (write-biginteger out x)
    (instance? clojure.lang.BigInt x) (write-biginteger out (biginteger x))

    (instance? Double x) (write-double-value out x)
    (instance? Float x) (write-float-value out x)
    (instance? java.math.BigDecimal x) (write-decimal out x)

    (uuid? x)
    (do (.writeByte out type-uuid)
        (write-be out (.getMostSignificantBits ^java.util.UUID x) 8)
        (write-be out (.getLeastSignificantBits ^java.util.UUID x) 8))

    (instance? java.util.List x)
    (do (.writeByte out type-nested)
        (doseq [item x]
          (write-value out item true))
        (.writeByte out 0x00))

    :else
    ;; reject instead of guessing - the FoundationDB Java tuple layer's
    ;; Number.longValue() fallback silently truncated BigDecimals to longs
    (throw (ex-info "Unsupported tuple value type"
                    {:value x
                     :type (class x)}))))

(defn supported-value?
  "True when [[pack]] can encode `x` as a tuple component - the single
   source of truth for the supported-type policy. Callers validate against
   this at their own boundary with their own error context; `write-value`'s
   throw remains the backstop."
  [x]
  (or (nil? x)
      (string? x)
      (bytes? x)
      (instance? Boolean x)
      (instance? Long x)
      (instance? Integer x)
      (instance? Short x)
      (instance? Byte x)
      (instance? java.math.BigInteger x)
      (instance? clojure.lang.BigInt x)
      (instance? Double x)
      (instance? Float x)
      (instance? java.math.BigDecimal x)
      (uuid? x)
      (instance? java.util.List x)))

(defn pack
  "Encodes the tuple `components` (a sequential collection) into a byte
   array that sorts in unsigned lexicographic byte order."
  ^bytes [components]
  (let [out (ByteBuf. (byte-array 64) 0)]
    (doseq [x components]
      (write-value out x false))
    (.toBytes out)))

;; ---------------------------------------------------------------------------
;; decoding

(defn- unescaped-length
  "Length of the escaped region starting at `pos` (exclusive of the
   terminator), in unescaped bytes; also returns the position after the
   terminator."
  [^bytes bs ^long pos]
  (let [n (alength bs)]
    (loop [i pos
           len 0]
      (if (zero? (aget bs i))
        (if (and (< (inc i) n)
                 (= -1 (aget bs (inc i)))) ; 0xff as signed byte
          (recur (+ i 2) (inc len))
          [len (inc i)])
        (recur (inc i) (inc len))))))

(defn- read-escaped
  "Reads an escaped region starting at `pos`. Returns [bytes end-pos]."
  [^bytes bs ^long pos]
  (let [[len end] (unescaped-length bs pos)
        len (long len)
        end (long end)]
    (if (= len (- end 1 pos))
      ;; no escapes (the common case - strings and hashes almost never
      ;; contain NUL): one bulk copy instead of a byte-at-a-time loop
      [(java.util.Arrays/copyOfRange bs pos (dec end)) end]
      (let [result (byte-array len)]
        (loop [i pos
               o 0]
          (when (< o len)
            (let [b (aget bs i)]
              (aset result o b)
              (recur (if (zero? b) (+ i 2) (inc i))
                     (inc o)))))
        [result end]))))

(defn- read-escaped-string
  "Reads an escaped region as a UTF-8 string. In the no-escape case the
   String is built directly on the source array, skipping the intermediate
   byte array `read-escaped` would allocate."
  [^bytes bs ^long pos]
  (let [[len end] (unescaped-length bs pos)
        len (long len)
        end (long end)]
    (if (= len (- end 1 pos))
      [(String. bs (int pos) (int len) java.nio.charset.StandardCharsets/UTF_8) end]
      (let [[^bytes raw end] (read-escaped bs pos)]
        [(String. raw java.nio.charset.StandardCharsets/UTF_8) end]))))

(defn- read-be
  "Reads `len` bytes big-endian as an unsigned long (len <= 8)."
  ^long [^bytes bs ^long pos ^long len]
  (loop [i 0
         acc 0]
    (if (< i len)
      (recur (inc i)
             (bit-or (bit-shift-left acc 8)
                     (long (bit-and 0xff (aget bs (+ pos i))))))
      acc)))

(defn- narrow-biginteger
  [^java.math.BigInteger n]
  (if (<= (.bitLength n) 63)
    (.longValueExact n)
    n))

(defn- read-biginteger
  "Reads `len` magnitude bytes as an unsigned BigInteger."
  ^java.math.BigInteger [^bytes bs ^long pos ^long len]
  (java.math.BigInteger. 1 bs (int pos) (int len)))

(defn- read-int-value
  "Reads an integer for type `code` at `pos` (after the code byte).
   Returns [value end-pos]."
  [^bytes bs ^long pos ^long code]
  (cond
    (= code type-int-zero)
    [0 pos]

    (< type-int-zero code type-pos-bignum)
    (let [len (- code type-int-zero)]
      [(if (or (< len 8)
               (zero? (bit-and 0x80 (aget bs pos))))
         (read-be bs pos len)
         ;; 8 bytes with the top bit set exceed Long/MAX_VALUE
         (narrow-biginteger (read-biginteger bs pos len)))
       (+ pos len)])

    (< type-neg-bignum code type-int-zero)
    (let [len (- type-int-zero code)]
      [(if (= len 8)
         ;; an 8-byte negative magnitude can lie below Long/MIN_VALUE,
         ;; so undo the offset in BigInteger arithmetic
         (narrow-biginteger
           (.subtract ^java.math.BigInteger (read-biginteger bs pos 8)
                      (.subtract (.shiftLeft java.math.BigInteger/ONE 64)
                                 java.math.BigInteger/ONE)))
         (- (read-be bs pos len)
            (dec (bit-shift-left 1 (* 8 len)))))
       (+ pos len)])

    (= code type-pos-bignum)
    (let [len (long (bit-and 0xff (aget bs pos)))
          pos (inc pos)]
      [(narrow-biginteger (read-biginteger bs pos len))
       (+ pos len)])

    (= code type-neg-bignum)
    (let [len (bit-xor (long (bit-and 0xff (aget bs pos))) 0xff)
          pos (inc pos)
          offset (.subtract (.shiftLeft java.math.BigInteger/ONE (int (* 8 len)))
                            java.math.BigInteger/ONE)]
      [(narrow-biginteger (.subtract ^java.math.BigInteger (read-biginteger bs pos len)
                                     offset))
       (+ pos len)])))

(defn- read-decimal
  "Reads a decimal at `pos` (after the type code). Returns [value end-pos]."
  [^bytes bs ^long pos]
  (let [marker (long (bit-and 0xff (aget bs pos)))
        pos (inc pos)]
    (if (= marker decimal-zero)
      [java.math.BigDecimal/ZERO pos]
      (let [mask (case marker
                   0x82 0x00
                   0x7e 0xff)
            exponent (- (bit-xor (read-be bs pos 4)
                                 (* mask 0x01010101))
                        0x80000000)
            terminator (bit-xor 0x00 mask)
            digits (StringBuilder.)
            end (loop [i (+ pos 4)]
                  (let [b (long (bit-and 0xff (aget bs i)))]
                    (if (= b terminator)
                      (inc i)
                      (do (.append digits
                                   (char (+ (int \0)
                                            (dec (bit-xor b mask)))))
                          (recur (inc i))))))
            unscaled (java.math.BigInteger. (str digits))
            scale (- (.length digits) exponent)
            abs (restore-plain-integer
                  (java.math.BigDecimal. unscaled (int scale)))]
        [(if (= mask 0xff) (.negate abs) abs)
         end]))))

(declare read-value)

(defn- read-nested
  [^bytes bs ^long pos]
  (loop [pos pos
         acc (transient [])]
    (let [b (long (bit-and 0xff (aget bs pos)))]
      (cond
        ;; 0x00 0xff is a null element, plain 0x00 terminates the nesting
        (and (= b type-null)
             (< (inc pos) (alength bs))
             (= 0xff (long (bit-and 0xff (aget bs (inc pos))))))
        (recur (+ pos 2) (conj! acc nil))

        (= b type-null)
        [(persistent! acc) (inc pos)]

        :else
        (let [[v pos] (read-value bs pos)]
          (recur (long pos) (conj! acc v)))))))

(defn- read-value
  "Reads one value at `pos`. Returns [value end-pos]."
  [^bytes bs ^long pos]
  (let [code (long (bit-and 0xff (aget bs pos)))
        pos (inc pos)]
    (cond
      (= code type-null) [nil pos]
      (= code type-true) [true pos]
      (= code type-false) [false pos]

      (= code type-string)
      (read-escaped-string bs pos)

      (= code type-bytes)
      (read-escaped bs pos)

      (= code type-nested)
      (read-nested bs pos)

      (<= type-neg-bignum code type-pos-bignum)
      (read-int-value bs pos code)

      (= code type-double)
      (let [bits (read-be bs pos 8)
            bits (if (neg? bits)
                   (bit-xor bits Long/MIN_VALUE)
                   (bit-not bits))]
        [(Double/longBitsToDouble bits) (+ pos 8)])

      (= code type-float)
      (let [bits (unchecked-int (read-be bs pos 4))
            bits (if (neg? bits)
                   (bit-xor bits Integer/MIN_VALUE)
                   (bit-not bits))]
        [(Float/intBitsToFloat (unchecked-int bits)) (+ pos 4)])

      (= code type-decimal)
      (read-decimal bs pos)

      (= code type-uuid)
      [(java.util.UUID. (read-be bs pos 8)
                        (read-be bs (+ pos 8) 8))
       (+ pos 16)]

      :else
      (throw (ex-info "Unsupported tuple type code"
                      {:code code
                       :pos (dec pos)})))))

(defn unpack
  "Decodes a byte array produced by `pack` back into a vector of components.
   Nested tuples decode as vectors."
  [^bytes bs]
  (loop [pos 0
         acc (transient [])]
    (if (< pos (alength bs))
      (let [[v pos] (read-value bs pos)]
        (recur (long pos) (conj! acc v)))
      (persistent! acc))))

;; ---------------------------------------------------------------------------
;; ranges

(defn- with-suffix
  ^bytes [^bytes prefix ^long b]
  (let [n (alength prefix)
        result (java.util.Arrays/copyOf prefix (inc n))]
    (aset result n (unchecked-byte b))
    result))

(defn range
  "Returns [begin end) bounds covering exactly the tuples that extend
   `components` with at least one more element (like FoundationDB's
   Tuple.range): every encoded element starts with a type code byte, which
   is > 0x00 or the null code 0x00 itself, and always < 0xFF."
  [components]
  (let [prefix (pack components)]
    [(with-suffix prefix 0x00)
     (with-suffix prefix 0xff)]))
