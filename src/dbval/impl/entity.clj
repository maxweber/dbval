(ns ^:no-doc dbval.impl.entity
  (:refer-clojure :exclude [keys get])
  (:require [clojure.core :as c]
    [dbval.db :as db]))

(declare entity ->Entity equiv-entity lookup-entity touch hash-entity)

(defn- entid [db eid]
  (when (or (uuid? eid)
          (sequential? eid)
          (keyword? eid))
    (db/entid db eid)))

(defn entity [db eid]
  {:pre [(db/db? db)]}
  (when-let [e (entid db eid)]
    (when (db/eid-exists? db e)
      (->Entity db e (volatile! false) (volatile! {})))))

(defn- ref-value
  "Datomic parity: a reference to an entity that has a `:db/ident` is
   represented by the ident keyword instead of an entity."
  [db eid]
  (if-some [ident (:v (first (db/-search db [eid :db/ident])))]
    ident
    (entity db eid)))

(defn- entity-attr [db a datoms]
  (if (db/multival? db a)
    (if (db/ref? db a)
      (reduce #(conj %1 (ref-value db (:v %2))) #{} datoms)
      (reduce #(conj %1 (:v %2)) #{} datoms))
    (if (db/ref? db a)
      (ref-value db (:v (first datoms)))
      (:v (first datoms)))))

(defn- -lookup-backwards [db eid attr not-found]
  (if-let [datoms (not-empty (db/-search db [nil attr eid]))]
    (if (db/component? db attr)
      (entity db (:e (first datoms)))
      (reduce #(conj %1 (entity db (:e %2))) #{} datoms))
    not-found))


(deftype Entity [db eid touched cache]
  Object
       (toString [e]      (pr-str (assoc @cache :db/id eid)))
       (hashCode [e]      (hash-entity e))
       (equals [e o]      (equiv-entity e o))

       clojure.lang.Seqable
       (seq [e]           (touch e) (seq @cache))

       clojure.lang.Associative
       (equiv [e o]       (equiv-entity e o))
       (containsKey [e k] (not= ::nf (lookup-entity e k ::nf)))
       (entryAt [e k]     (some->> (lookup-entity e k) (clojure.lang.MapEntry. k)))

       (empty [e]         (throw (UnsupportedOperationException.)))
       (assoc [e k v]     (throw (UnsupportedOperationException.)))
       (cons  [e [k v]]   (throw (UnsupportedOperationException.)))
       (count [e]         (touch e) (count @(.-cache e)))

       clojure.lang.ILookup
       (valAt [e k]       (lookup-entity e k))
       (valAt [e k not-found] (lookup-entity e k not-found))

       clojure.lang.IFn
       (invoke [e k]      (lookup-entity e k))
       (invoke [e k not-found] (lookup-entity e k not-found)))

(defn entity? [x] (instance? Entity x))


(defmethod print-method Entity [e, ^java.io.Writer w]
     (.write w (str e)))

(defn- equiv-entity [^Entity this that]
  (and
    (instance? Entity that)
    (identical? (.-db this) (.-db ^Entity that)) ; `=` and `hash` on db is expensive
    (= (.-eid this) (.-eid ^Entity that))))

(defn- hash-entity [^Entity e]
  (db/combine-hashes
    (hash (.-eid e))
    ;; A hash compatible with `identical?`. Consistent with `=`.
    (System/identityHashCode (.-db e))))

(defn- lookup-entity
  ([this attr] (lookup-entity this attr nil))
  ([^Entity this attr not-found]
   (if (= attr :db/id)
     (.-eid this)
     (if (db/reverse-ref? attr)
       (-lookup-backwards (.-db this) (.-eid this) (db/reverse-ref attr) not-found)
       (if-some [v (@(.-cache this) attr)]
         v
         (if @(.-touched this)
           not-found
           (if-some [datoms (not-empty (db/-search (.-db this) [(.-eid this) attr]))]
             (let [value (entity-attr (.-db this) attr datoms)]
               (vreset! (.-cache this) (assoc @(.-cache this) attr value))
               value)
             not-found)))))))

(defn touch-components [db a->v]
  (reduce-kv
    (fn [acc a v]
      (assoc acc a
        (if (db/component? db a)
          (if (db/multival? db a)
            (set (map touch v))
            (touch v))
          v)))
    {} a->v))

(defn- datoms->cache [db datoms]
  (reduce (fn [acc part]
            (let [a (:a (first part))]
              (assoc acc a (entity-attr db a part))))
    {} (partition-by :a datoms)))

(defn touch [^Entity e]
  {:pre [(or (nil? e) (entity? e))]}
  (when (some? e)
    (when-not @(.-touched e)
      (when-let [datoms (not-empty (db/-search (.-db e) [(.-eid e)]))]
        (vreset! (.-cache e) (->> datoms
                               (datoms->cache (.-db e))
                               (touch-components (.-db e))))
        (vreset! (.-touched e) true)))
    e))

