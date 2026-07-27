(ns dbval.store
  "Storage abstraction for dbval.

   A store is an ordered set of byte-array keys (FoundationDB-tuple encoded
   datoms, see `dbval.db`) that supports range scans over committed data and
   atomic batch commits. Conceptually the store is a sorted set, mimicking a
   transactional ordered key-value store like FoundationDB.

   Datoms of deref attributes (see `dbval.db`) keep only a content hash in
   their keys; the value bytes live in a separate content-addressed blob
   area: `-commit!` takes the batch's blobs alongside its keys and
   `-get-blob` reads one back by hash. Blobs are immutable — the same hash
   always maps to the same bytes, so re-writing an existing blob is a no-op.

   Stores never see uncommitted state: read-your-writes inside a running
   transaction is handled by the engine (`dbval.db`), which overlays the
   transaction's pending keys and blobs over `-scan`/`-get-blob`. A store
   implementation therefore only has to provide committed data.

   Implementations: `dbval.store.sqlite` (default), `dbval.store.memory`.")

(defprotocol ITupleStore
  (-scan [store begin end reverse?]
    "Returns an Iterable/seqable of byte[] keys k with begin <= k < end,
     compared in unsigned byte order, ascending — or descending when
     `reverse?`. Only committed keys are visible.")
  (-commit! [store keys blobs]
    "Atomically adds the byte[] `keys` and the `blobs` (a java.util.Map of
     byte[] content hash -> byte[] value) to the store: after `-commit!`
     returns, either all keys and blobs are durably visible or — if it
     throws — none are. Keys and blobs that already exist are ignored.")
  (-get-blob [store hash]
    "Returns the committed byte[] blob stored under the byte[] content
     `hash`, or nil if there is none.")
  (-close! [store]
    "Releases the store's resources."))

(defn scan
  "See [[ITupleStore]]."
  [store begin end reverse?]
  (-scan store begin end reverse?))

(defn commit!
  "See [[ITupleStore]]."
  [store keys blobs]
  (-commit! store keys blobs))

(defn get-blob
  "See [[ITupleStore]]."
  [store hash]
  (-get-blob store hash))

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
