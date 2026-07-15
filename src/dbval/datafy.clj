(ns dbval.datafy
  (:require
    [clojure.core.protocols :as cp]
    [dbval.pull-api :as dp]
    [dbval.db :as db]
    [dbval.impl.entity :as e]))

(declare datafy-entity-seq)

(defn- navize-pulled-entity [db-val pulled-entity]
  (let [ref-attrs (db/-attrs-by db-val :db.type/ref)
        ref-rattrs (set (map db/reverse-ref ref-attrs))
        many-attrs (db/-attrs-by db-val :db.cardinality/many)]
    (with-meta pulled-entity
      {`cp/nav (fn [coll k v]
                 (cond
                   (or (and (many-attrs k) (ref-attrs k))
                     (ref-rattrs k))
                   (datafy-entity-seq db-val v)
                   (ref-attrs k)
                   (e/entity db-val (:db/id v))
                   :else v))})))

(defn- navize-pulled-entity-seq [db-val entities]
  (with-meta entities
    {`cp/nav (fn [coll k v]
               (e/entity db-val (:db/id v)))}))

(defn- datafy-entity-seq [db-val entities]
  (with-meta entities
    {`cp/datafy (fn [entities] (navize-pulled-entity-seq db-val entities))}))

(extend-protocol cp/Datafiable
  dbval.impl.entity.Entity
  (datafy [this]
    (let [db (.-db this)
          ref-attrs (db/-attrs-by db :db.type/ref)
          ref-rattrs (set (map db/reverse-ref ref-attrs))
          pull-pattern (into ["*"] ref-rattrs)]
      (navize-pulled-entity db (dp/pull db pull-pattern (:db/id this))))))
