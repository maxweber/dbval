(ns dbval.test.lookup-refs
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc])
  (:import
       [clojure.lang ExceptionInfo]))

(deftest test-lookup-refs
  (let [tx (d/with (d/empty-db {:name  {:db/unique :db.unique/identity}
                                :email {:db/unique :db.unique/value}})
             [{:db/id "e1" :name "Ivan" :email "@1" :age 35}
              {:db/id "e2" :name "Petr" :email "@2" :age 22}])
        e1 (get (:tempids tx) "e1")
        db (:db-after tx)]

    (are [eid res] (= (tdc/entity-map db eid) res)
      [:name "Ivan"]   {:db/id e1 :name "Ivan" :email "@1" :age 35}
      [:email "@1"]    {:db/id e1 :name "Ivan" :email "@1" :age 35}
      [:name "Sergey"] nil
      [:name nil]      nil)

    (are [eid msg] (thrown-msg? msg (d/entity db eid))
      [:name]     "Lookup ref should contain 2 elements: [:name]"
      [:name 1 2] "Lookup ref should contain 2 elements: [:name 1 2]"
      [:age 10]   "Lookup ref attribute should be marked as :db/unique: [:age 10]")))

(deftest test-lookup-refs-transact
  (let [schema {:name {:db/unique :db.unique/identity}
                :friend {:db/valueType :db.type/ref}}
        setup (fn []
                (let [tx (d/with (d/empty-db schema)
                                 [{:db/id "e1" :name "Ivan"}
                                  {:db/id "e2" :name "Petr"}])]
                  {:db (:db-after tx)
                   :e1 (get (:tempids tx) "e1")
                   :e2 (get (:tempids tx) "e2")}))]

    (testing "Add via lookup ref"
      (let [{:keys [db e1 e2]} (setup)
            db' (d/db-with db [[:db/add [:name "Ivan"] :age 35]])]
        (is (= {:db/id e1 :name "Ivan" :age 35}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Add via entity map with lookup ref"
      (let [{:keys [db e1]} (setup)
            db' (d/db-with db [{:db/id [:name "Ivan"] :age 35}])]
        (is (= {:db/id e1 :name "Ivan" :age 35}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Add friend ref via lookup"
      (let [{:keys [db e1 e2]} (setup)
            db' (d/db-with db [[:db/add [:name "Ivan"] :friend [:name "Petr"]]])]
        (is (= {:db/id e1 :name "Ivan" :friend {:db/id e2}}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Add friend via entity map"
      (let [{:keys [db e1 e2]} (setup)
            db' (d/db-with db [{:db/id [:name "Ivan"] :friend [:name "Petr"]}])]
        (is (= {:db/id e1 :name "Ivan" :friend {:db/id e2}}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Reverse ref"
      (let [{:keys [db e1 e2]} (setup)
            db' (d/db-with db [{:db/id [:name "Petr"] :_friend [:name "Ivan"]}])]
        (is (= {:db/id e1 :name "Ivan" :friend {:db/id e2}}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Lookup ref resolved at intermediate DB"
      (let [{:keys [db e1]} (setup)
            tx (d/with db [{:db/id "e3" :name "Oleg"}
                           [:db/add [:name "Ivan"] :friend [:name "Oleg"]]])
            e3 (get (:tempids tx) "e3")]
        (is (= {:db/id e1 :name "Ivan" :friend {:db/id e3}}
               (tdc/entity-map (:db-after tx) [:name "Ivan"])))))

    (testing "CAS with lookup ref"
      (let [{:keys [db e1]} (setup)
            db' (d/db-with db [[:db.fn/cas [:name "Ivan"] :name "Ivan" "Oleg"]])]
        (is (= {:db/id e1 :name "Oleg"}
               (tdc/entity-map db' e1)))))

    (testing "Retract via lookup ref"
      (let [{:keys [db e1]} (setup)
            db' (d/db-with db [[:db/add [:name "Ivan"] :age 35]
                               [:db/retract [:name "Ivan"] :age 35]])]
        (is (= {:db/id e1 :name "Ivan"}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "RetractAttribute via lookup ref"
      (let [{:keys [db e1]} (setup)
            db' (d/db-with db [[:db/add [:name "Ivan"] :age 35]
                               [:db.fn/retractAttribute [:name "Ivan"] :age]])]
        (is (= {:db/id e1 :name "Ivan"}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "RetractEntity via lookup ref"
      (let [{:keys [db]} (setup)
            db' (d/db-with db [[:db.fn/retractEntity [:name "Ivan"]]])]
        (is (nil? (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Error for non-existent lookup ref"
      (let [{:keys [db]} (setup)]
        (is (thrown-msg? "Nothing found for entity id [:name \"Oleg\"]"
              (d/db-with db [{:db/id [:name "Oleg"], :age 10}])))
        (is (thrown-msg? "Nothing found for entity id [:name \"Oleg\"]"
              (d/db-with db [[:db/add [:name "Oleg"] :age 10]])))))))

(deftest test-lookup-refs-transact-multi
  (let [schema {:name {:db/unique :db.unique/identity}
                :friends {:db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/many}}
        setup (fn []
                (let [tx (d/with (d/empty-db schema)
                                 [{:db/id "e1" :name "Ivan"}
                                  {:db/id "e2" :name "Petr"}
                                  {:db/id "e3" :name "Oleg"}
                                  {:db/id "e4" :name "Sergey"}])]
                  {:db (:db-after tx)
                   :e1 (get (:tempids tx) "e1")
                   :e2 (get (:tempids tx) "e2")
                   :e3 (get (:tempids tx) "e3")
                   :e4 (get (:tempids tx) "e4")}))]

    (testing "Add one friend via lookup ref"
      (let [{:keys [db e1 e2]} (setup)
            db' (d/db-with db [[:db/add [:name "Ivan"] :friends [:name "Petr"]]])]
        (is (= {:db/id e1 :name "Ivan" :friends #{{:db/id e2}}}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Add multiple friends via lookup refs"
      (let [{:keys [db e1 e2 e3]} (setup)
            db' (d/db-with db [[:db/add [:name "Ivan"] :friends [:name "Petr"]]
                               [:db/add [:name "Ivan"] :friends [:name "Oleg"]]])]
        (is (= {:db/id e1 :name "Ivan" :friends #{{:db/id e2} {:db/id e3}}}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Add friend via entity map with single lookup ref"
      (let [{:keys [db e1 e2]} (setup)
            db' (d/db-with db [{:db/id [:name "Ivan"] :friends [:name "Petr"]}])]
        (is (= {:db/id e1 :name "Ivan" :friends #{{:db/id e2}}}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Add friends via entity map with list of lookup refs"
      (let [{:keys [db e1 e2 e3]} (setup)
            db' (d/db-with db [{:db/id [:name "Ivan"] :friends [[:name "Petr"] [:name "Oleg"]]}])]
        (is (= {:db/id e1 :name "Ivan" :friends #{{:db/id e2} {:db/id e3}}}
               (tdc/entity-map db' [:name "Ivan"])))))

    (testing "Reverse ref multi"
      (let [{:keys [db e1 e2]} (setup)
            db' (d/db-with db [{:db/id [:name "Petr"] :_friends [:name "Ivan"]}])]
        (is (= {:db/id e1 :name "Ivan" :friends #{{:db/id e2}}}
               (tdc/entity-map db' [:name "Ivan"])))))))

(deftest lookup-refs-index-access
  ;; Test lookup refs in index access - use queries instead of hardcoded IDs
  (let [schema {:name {:db/unique :db.unique/identity}
                :friends {:db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/many}}
        setup (fn []
                (let [tx (d/with (d/empty-db schema)
                                 [{:db/id "e1" :name "Ivan" :friends ["e2" "e3"]}
                                  {:db/id "e2" :name "Petr" :friends "e3"}
                                  {:db/id "e3" :name "Oleg"}])]
                  {:db (:db-after tx)
                   :e1 (get (:tempids tx) "e1")
                   :e2 (get (:tempids tx) "e2")
                   :e3 (get (:tempids tx) "e3")}))]

    (testing "datoms with lookup ref"
      (let [{:keys [db e1 e2 e3]} (setup)]
        ;; :eavt with lookup ref
        (is (= #{[:friends e2] [:friends e3] [:name "Ivan"]}
               (set (map (juxt :a :v) (d/datoms db :eavt [:name "Ivan"])))))
        ;; :eavt with lookup ref and attr
        (is (= #{[e2] [e3]}
               (set (map (comp vector :v) (d/datoms db :eavt [:name "Ivan"] :friends)))))
        ;; :eavt with lookup ref, attr, and value lookup ref
        (is (= #{[e1 :friends e2]}
               (set (map (juxt :e :a :v) (d/datoms db :eavt [:name "Ivan"] :friends [:name "Petr"])))))
        ;; :aevt with attr and entity lookup ref
        (is (= #{[e2] [e3]}
               (set (map (comp vector :v) (d/datoms db :aevt :friends [:name "Ivan"])))))
        ;; :avet with value lookup ref
        (is (= #{[e1] [e2]}
               (set (map (comp vector :e) (d/datoms db :avet :friends [:name "Oleg"])))))))

    (testing "seek-datoms with lookup ref"
      (let [{:keys [db e1]} (setup)]
        ;; Verify lookup ref resolves correctly
        (is (= (vec (d/seek-datoms db :eavt [:name "Ivan"]))
               (vec (d/seek-datoms db :eavt e1))))))

    (testing "index-range with lookup refs"
      (let [{:keys [db e1 e2 e3]} (setup)]
        ;; index-range for :friends with value lookup refs
        (is (= #{[e1 :friends e3] [e2 :friends e3]}
               (set (map (juxt :e :a :v) (d/index-range db :friends [:name "Oleg"] [:name "Oleg"])))))))))

(deftest test-lookup-refs-query
  (let [schema {:name {:db/unique :db.unique/identity}
                :friend {:db/valueType :db.type/ref}
                :id {:db/unique :db.unique/identity}}
        tx (d/with (d/empty-db schema)
             [{:db/id "e1" :id 1 :name "Ivan" :age 11 :friend "e2"}
              {:db/id "e2" :id 2 :name "Petr" :age 22 :friend "e3"}
              {:db/id "e3" :id 3 :name "Oleg" :age 33}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]

    (testing "Query with lookup ref input"
      (is (= #{[[:name "Ivan"] 11]}
             (set (d/q '[:find ?e ?v
                         :in $ ?e
                         :where [?e :age ?v]]
                    db [:name "Ivan"])))))

    (testing "Query with multiple lookup ref inputs"
      (is (= #{11 22}
             (set (d/q '[:find [?v ...]
                         :in $ [?e ...]
                         :where [?e :age ?v]]
                    db [[:name "Ivan"] [:name "Petr"]])))))

    (testing "Query friend by value lookup ref"
      (is (= #{e1}
             (set (d/q '[:find [?e ...]
                         :in $ ?v
                         :where [?e :friend ?v]]
                    db [:name "Petr"])))))

    (testing "Query friend by multiple value lookup refs"
      (is (= #{e1 e2}
             (set (d/q '[:find [?e ...]
                         :in $ [?v ...]
                         :where [?e :friend ?v]]
                    db [[:name "Petr"] [:name "Oleg"]])))))

    (testing "Query returns lookup refs"
      (is (= #{[[:name "Ivan"] [:name "Petr"]]}
             (d/q '[:find ?e ?v
                    :in $ ?e ?v
                    :where [?e :friend ?v]]
               db [:name "Ivan"] [:name "Petr"]))))

    (testing "Cross-product of lookup refs"
      (is (= #{[[:name "Ivan"] [:name "Petr"]]
               [[:name "Petr"] [:name "Oleg"]]}
             (d/q '[:find ?e ?v
                    :in $ [?e ...] [?v ...]
                    :where [?e :friend ?v]]
               db [[:name "Ivan"] [:name "Petr"] [:name "Oleg"]]
               [[:name "Ivan"] [:name "Petr"] [:name "Oleg"]]))))

    ;; issue-214 - query with mixed entity inputs
    (testing "Query with mixed entity inputs"
      (is (= #{[e2]}
             (d/q '[:find ?e
                    :in $ [?e ...] ?friend
                    :where [?e :friend ?friend]]
               db [e1 e2 e3] e3))))

    (testing "inline refs in where clause"
      (is (= #{[e2]}
             (d/q '[:find ?v
                    :where [[:name "Ivan"] :friend ?v]]
               db)))

      (is (= #{[e1]}
             (d/q '[:find ?e
                    :where [?e :friend [:name "Petr"]]]
               db)))

      (is (thrown-msg? "Nothing found for entity id [:name \"Valery\"]"
            (d/q '[:find ?e
                   :where [[:name "Valery"] :friend ?e]]
              db))))))
