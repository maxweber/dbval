(ns dbval.store
  "Storage abstraction for dbval.

   A store is an ordered set of byte-array keys (FoundationDB-tuple encoded
   datoms, see `dbval.db`) that supports range scans over committed data and
   atomic batch commits. dbval only needs the key portion: conceptually the
   store is a sorted set, mimicking a transactional ordered key-value store
   like FoundationDB.

   Stores never see uncommitted state: read-your-writes inside a running
   transaction is handled by the engine (`dbval.db`), which overlays the
   transaction's pending keys over `-scan`. A store implementation therefore
   only has to provide:

   - `-scan`: committed keys in unsigned byte order
   - `-commit!`: atomically add a batch of keys (all or nothing)

   Implementations: `dbval.store.sqlite` (default), `dbval.store.memory`.")

(defprotocol ITupleStore
  (-scan [store begin end reverse?]
    "Returns an Iterable/seqable of byte[] keys k with begin <= k < end,
     compared in unsigned byte order, ascending — or descending when
     `reverse?`. Only committed keys are visible.")
  (-commit! [store keys]
    "Atomically adds the byte[] `keys` to the store: after `-commit!`
     returns, either all keys are durably visible to subsequent scans or —
     if it throws — none are. Keys that already exist are ignored.")
  (-close! [store]
    "Releases the store's resources."))

(defn scan
  "See [[ITupleStore]]."
  [store begin end reverse?]
  (-scan store begin end reverse?))

(defn commit!
  "See [[ITupleStore]]."
  [store keys]
  (-commit! store keys))

(defn close!
  "See [[ITupleStore]]."
  [store]
  (-close! store))

(defn byte-array-compare
  ^long [^bytes a ^bytes b]
  (java.util.Arrays/compareUnsigned a b))

(def byte-array-comparator
  "Unsigned lexicographic byte[] comparator — the key order every store
   must scan in."
  (reify java.util.Comparator
    (compare [_ a b]
      (byte-array-compare ^bytes a ^bytes b))))
