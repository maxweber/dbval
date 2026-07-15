(ns ^:no-doc dbval.lru)

(declare assoc-lru cleanup-lru)

(deftype LRU [^clojure.lang.Associative key-value gen-key key-gen gen limit]
     clojure.lang.ILookup
     (valAt [_ k]           (.valAt key-value k))
     (valAt [_ k not-found] (.valAt key-value k not-found))
     clojure.lang.Associative
     (containsKey [_ k] (.containsKey key-value k))
     (entryAt [_ k]     (.entryAt key-value k))
     (assoc [this k v]  (assoc-lru this k v)))

(defn assoc-lru [^LRU lru k v]
  (let [key-value (.-key-value lru)
        gen-key   (.-gen-key lru)
        key-gen   (.-key-gen lru)
        gen       (.-gen lru)
        limit     (.-limit lru)]
    (if-let [g (key-gen k nil)]
      (LRU.
        key-value
        (-> gen-key
          (dissoc g)
          (assoc gen k))
        (assoc key-gen k gen)
        (inc gen)
        limit)
      (cleanup-lru
        (LRU.
          (assoc key-value k v)
          (assoc gen-key gen k)
          (assoc key-gen k gen)
          (inc gen)
          limit)))))

(defn cleanup-lru [^LRU lru]
  (if (> (count (.-key-value lru)) (.-limit lru))
    (let [key-value (.-key-value lru)
          gen-key   (.-gen-key lru)
          key-gen   (.-key-gen lru)
          gen       (.-gen lru)
          limit     (.-limit lru)
          [g k]     (first gen-key)]
      (LRU.
        (dissoc key-value k)
        (dissoc gen-key g)
        (dissoc key-gen k)
        gen
        limit))
    lru))

(defn lru [limit]
  (LRU. {} (sorted-map) {} 0 limit))

(defprotocol ICache
  (-get [this key compute-fn]))

(defn cache [limit]
  (let [*impl (volatile! (lru limit))]
    (reify ICache
      (-get [_ key compute-fn]
        (if-some [cached (get @*impl key nil)]
          (do (vswap! *impl assoc key cached)
            cached)
          (let [computed (compute-fn)]
            (vswap! *impl assoc key computed)
            computed))))))
