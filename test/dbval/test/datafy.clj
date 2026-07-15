(ns dbval.test.datafy
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.datafy :as datafy]
    [dbval.core :as d]
    [clojure.core.protocols :as cp]
    [dbval.impl.entity :as e]))

(defn- make-test-db []
  (let [schema {:name {:db/unique :db.unique/identity}
                :ref {:db/valueType :db.type/ref}
                :namespace/ref {:db/valueType :db.type/ref}
                :many/ref {:db/valueType :db.type/ref
                           :db/cardinality :db.cardinality/many}}
        tx (d/with (d/empty-db schema)
             [{:db/id "e1" :name "Parent1"}
              {:db/id "e2" :name "Child1" :ref "e1" :namespace/ref "e1"}
              {:db/id "e3" :name "GrandChild1" :ref "e2" :namespace/ref "e2"}
              {:db/id "e4" :name "Master" :many/ref ["e1" "e2" "e3"]}])]
    {:db (:db-after tx)
     :tempids (:tempids tx)}))

(def ^:private *test-data (delay (make-test-db)))
(defn- test-db [] (:db @*test-data))
(defn- eid [name] (get (:tempids @*test-data) name))

(defn- nav [coll k]
  (cp/nav coll k (coll k)))

(defn d+n
  "Helper function to datafy/navigate for a path"
  [coll [k & ks]]
  (if (nil? k)
    coll
    (d+n (nav (cp/datafy coll) k)
      ks)))

(deftest test-navigation
  (let [e1 (eid "e1")
        e2 (eid "e2")
        e3 (eid "e3")
        entity (e/entity (test-db) e3)]
    (is (= e2 (:db/id (d+n entity [:ref]))))
    (is (= e2 (:db/id (d+n entity [:namespace/ref]))))
    (is (= e1 (:db/id (d+n entity [:ref :namespace/ref]))))
    (is (= e3 (:db/id (d+n entity [:namespace/ref :ref :_ref 0 :namespace/_ref 0]))))
    (is (= #{e1 e2 e3} (set (map :db/id (d+n entity [:many/_ref 0 :many/ref])))))))

