(ns dbval.test.entity
  (:require
    [clojure.edn :as edn]
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc])
  (:import
       [clojure.lang ExceptionInfo]))

(t/use-fixtures :once tdc/no-namespace-maps)

(deftest test-entity
  (let [tx (d/with (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
             [{:db/id "e1", :name "Ivan", :age 19, :aka ["X" "Y"]}
              {:db/id "e2", :name "Ivan", :sex "male", :aka ["Z"]}
              [:db/add "e3" :huh? false]])
        db (:db-after tx)
        e1-id (get (:tempids tx) "e1")
        e2-id (get (:tempids tx) "e2")
        e3-id (get (:tempids tx) "e3")
        e  (d/entity db e1-id)]
    (is (= (:db/id e) e1-id))
    (is (identical? (d/entity-db e) db))
    (is (= (:name e) "Ivan"))
    (is (= (e :name) "Ivan")) ; IFn form
    (is (= (:age  e) 19))
    (is (= (:aka  e) #{"X" "Y"}))
    (is (= true (contains? e :age)))
    (is (= false (contains? e :not-found)))
    (is (= (into {} e)
          {:name "Ivan", :age 19, :aka #{"X" "Y"}}))
    (is (= (into {} (d/entity db e1-id))
          {:name "Ivan", :age 19, :aka #{"X" "Y"}}))
    (is (= (into {} (d/entity db e2-id))
          {:name "Ivan", :sex "male", :aka #{"Z"}}))
    (let [e3 (d/entity db e3-id)]
      (is (= (into {} e3) {:huh? false})) ; Force caching.
      (is (false? (:huh? e3))))

    ;; With UUIDs, pr-str shows the UUID, not integer
    (is (uuid? (:db/id (d/entity db e1-id))))
    (is (= e1-id (:db/id (d/entity db e1-id))))
    ;; Read back entity and verify attributes
    (let [e (d/entity db e1-id)]
      (:name e)  ; touch to load
      (is (= (:name e) "Ivan")))))

(deftest test-entity-refs
  (let [tx (d/with (d/empty-db {:father   {:db/valueType   :db.type/ref}
                                 :children {:db/valueType   :db.type/ref
                                            :db/cardinality :db.cardinality/many}})
             [{:db/id "e1", :children ["e10"]}
              {:db/id "e10", :father "e1", :children ["e100" "e101"]}
              {:db/id "e100", :father "e10"}
              {:db/id "e101", :father "e10"}])
        db (:db-after tx)
        e1-id (get (:tempids tx) "e1")
        e10-id (get (:tempids tx) "e10")
        e100-id (get (:tempids tx) "e100")
        e101-id (get (:tempids tx) "e101")
        e  #(d/entity db %)]

    (is (= (:children (e e1-id))   #{(e e10-id)}))
    (is (= (:children (e e10-id))  #{(e e100-id) (e e101-id)}))

    (testing "empty attribute"
      (is (= (:children (e e100-id)) nil)))

    (testing "nested navigation"
      (is (= (-> (e e1-id) :children first :children) #{(e e100-id) (e e101-id)}))
      (is (= (-> (e e10-id) :children first :father) (e e10-id)))
      (is (= (-> (e e10-id) :father :children) #{(e e10-id)}))

      (testing "after touch"
        (let [e1  (e e1-id)
              e10 (e e10-id)]
          (d/touch e1)
          (d/touch e10)
          (is (= (-> e1 :children first :children) #{(e e100-id) (e e101-id)}))
          (is (= (-> e10 :children first :father) (e e10-id)))
          (is (= (-> e10 :father :children) #{(e e10-id)})))))

    (testing "backward navigation"
      (is (= (:_children (e e1-id))  nil))
      (is (= (:_father   (e e1-id))  #{(e e10-id)}))
      (is (= (:_children (e e10-id)) #{(e e1-id)}))
      (is (= (:_father   (e e10-id)) #{(e e100-id) (e e101-id)}))
      (is (= (-> (e e100-id) :_children first :_children) #{(e e1-id)})))))

(deftest test-missing-refs
  (let [schema {:ref       {:db/valueType   :db.type/ref}
                :comp      {:db/valueType   :db.type/ref
                            :db/isComponent true}
                :multiref  {:db/valueType   :db.type/ref
                            :db/cardinality :db.cardinality/many}
                :multicomp {:db/valueType   :db.type/ref
                            :db/isComponent true
                            :db/cardinality :db.cardinality/many}}
        db     (d/empty-db schema)
        ;; Use UUIDs for refs to non-existent entities
        missing-eid (random-uuid)
        tx     (d/with db
                 [[:db/add "e1" :ref       missing-eid]
                  [:db/add "e1" :comp      missing-eid]
                  [:db/add "e1" :multiref  missing-eid]
                  [:db/add "e1" :multiref  missing-eid]
                  [:db/add "e1" :multicomp missing-eid]
                  [:db/add "e1" :multicomp missing-eid]])
        db' (:db-after tx)
        e1-id (get (:tempids tx) "e1")]
    (d/touch (d/entity db e1-id)) ;; does not throw
    (is (= nil (:ref (d/entity db e1-id))))
    (is (= nil (:comp (d/entity db e1-id))))
    (is (= nil (:multiref (d/entity db e1-id))))
    (is (= nil (:multicomp (d/entity db e1-id))))))

(deftest test-entity-misses
  (let [tx (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
             [{:db/id "e1", :name "Ivan"}
              {:db/id "e2", :name "Oleg"}])
        db (:db-after tx)]
    (is (nil? (d/entity db nil)))
    (is (nil? (d/entity db "abc")))
    (is (nil? (d/entity db :keyword)))
    (is (nil? (d/entity db [:name "Petr"])))
    ;; With UUIDs, random UUIDs for non-existent entities return nil
    (is (nil? (d/entity db (random-uuid))))
    (is (thrown-msg? "Lookup ref attribute should be marked as :db/unique: [:not-an-attr 777]"
          (d/entity db [:not-an-attr 777])))))

(deftest test-entity-equality
  (let [tx (d/with (d/empty-db {})
             [{:db/id "e1", :name "Ivan"}])
        db1 (:db-after tx)
        e1-id (get (:tempids tx) "e1")
        e1  (d/entity db1 e1-id)
        db2 (d/db-with db1 [])
        db3 (d/db-with db2 [{:db/id "e2", :name "Oleg"}])]

    (testing "Two entities are equal if they have the same :db/id"
      (is (= e1 e1))
      (is (= e1 (d/entity db1 e1-id)))

      (testing "and refer to the same database"
        (is (not= e1 (d/entity db2 e1-id)))
        (is (not= e1 (d/entity db3 e1-id)))))))

(deftest test-entity-hash
  (let [tx (d/with (d/empty-db {})
             [{:db/id "e1", :name "Ivan"}])
        db1 (:db-after tx)
        e1-id (get (:tempids tx) "e1")
        e1  (d/entity db1 e1-id)
        db2 (d/db-with db1 [])
        db3 (d/db-with db1 [{:db/id "e2", :name "Oleg"}])]

    (testing "Two entities have the same hash if they have the same :db/id"
      (is (= (hash e1) (hash e1)))
      (is (= (hash e1) (hash (d/entity db1 e1-id))))

      (testing "and refer to the same database"
        (is (not= (hash e1) (hash (d/entity db2 e1-id))))
        (is (not= (hash e1) (hash (d/entity db3 e1-id))))))))
