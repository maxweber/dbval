(ns dbval.store.memory
  "In-memory tuple store: a concurrent sorted set of byte-array keys.

   Nothing is persisted — useful for tests and for exercising the engine
   without any storage backend. Scans return a live view of the set; that
   is safe because the engine filters every datom by the snapshot's
   `:max-tx`, so keys committed after a snapshot was taken are invisible
   to it regardless of when they appear in a scan."
  (:require
    [dbval.store :as store])
  (:import
    [java.util.concurrent ConcurrentSkipListSet]))

(deftype MemoryStore [^ConcurrentSkipListSet keyset]
  store/ITupleStore
  (-scan [_ begin end reverse?]
    (let [sub (.subSet keyset begin true end false)]
      (if reverse?
        (.descendingSet ^java.util.NavigableSet sub)
        sub)))

  (-commit! [this keys]
    ;; single writer at a time keeps the batch atomic with respect to other
    ;; commits; readers may observe a batch mid-insert, but the engine's
    ;; :max-tx filtering makes those keys invisible until the transaction's
    ;; basis is handed out
    (locking this
      (doseq [^bytes k keys]
        (.add keyset k))))

  (-close! [_] nil))

(defn store
  "Creates an empty in-memory tuple store."
  ^dbval.store.memory.MemoryStore []
  (MemoryStore. (ConcurrentSkipListSet. ^java.util.Comparator store/byte-array-comparator)))
