(ns dbval.test.explode
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))


(deftest test-explode
  (doseq [coll [["Devil" "Tupen"]
                #{"Devil" "Tupen"}
                '("Devil" "Tupen")
                (to-array ["Devil" "Tupen"])]]
    (testing coll
      (let [conn (d/create-conn {:aka  {:db/cardinality :db.cardinality/many}
                                 :also {:db/cardinality :db.cardinality/many}
                                 :name {:db/unique :db.unique/identity}})
            tx (d/transact! conn [{:db/id "e1"
                                   :name  "Ivan"
                                   :age   16
                                   :aka   coll
                                   :also  "ok"}])
            e1 (get (:tempids tx) "e1")]
        (is (= (d/q '[:find  ?n ?a
                      :in $ ?e
                      :where [?e :name ?n]
                      [?e :age ?a]] @conn e1)
              #{["Ivan" 16]}))
        (is (= (d/q '[:find  ?v
                      :in $ ?e
                      :where [?e :also ?v]] @conn e1)
              #{["ok"]}))
        (is (= (d/q '[:find  ?v
                      :in $ ?e
                      :where [?e :aka ?v]] @conn e1)
              #{["Devil"] ["Tupen"]}))))))

(deftest test-explode-ref
  (let [db0* (fn [] (d/empty-db {:children {:db/valueType :db.type/ref
                                            :db/cardinality :db.cardinality/many}
                                 :name {:db/unique :db.unique/identity}}))]
    ;; Test with string tempids in ref values
    (doseq [children [["e2" "e3"]
                      #{"e2" "e3"}
                      (list "e2" "e3")]]
      (testing (str "ref + many + " (type children))
        (let [db (d/db-with (db0*) [{:db/id "e1", :name "Ivan", :children children}
                                    {:db/id "e2", :name "Petr"}
                                    {:db/id "e3", :name "Evgeny"}])]
          (is (= #{["Petr"] ["Evgeny"]}
                (d/q '[:find ?n
                       :where
                       [_ :children ?e]
                       [?e :name ?n]] db))))))

    ;; Test with reverse ref
    (let [db (d/db-with (db0*) [{:db/id "e1", :name "Ivan"}
                                {:db/id "e2", :name "Petr", :_children "e1"}
                                {:db/id "e3", :name "Evgeny", :_children "e1"}])]
      (is (= #{["Petr"] ["Evgeny"]}
            (d/q '[:find ?n
                   :where
                   [_ :children ?e]
                   [?e :name ?n]] db))))

    (let [e1 (random-uuid)]
      (is (thrown-msg? "Bad attribute :_parent: reverse attribute name requires {:db/valueType :db.type/ref} in schema"
                       (d/db-with (db0*) [{:name "Sergey" :_parent e1}]))))))

(deftest test-explode-nested-maps
  ;; Test nested maps expand correctly - use relationship queries instead of hardcoded IDs
  (let [schema {:profile {:db/valueType :db.type/ref}
                :name {:db/unique :db.unique/identity}
                :email {:db/unique :db.unique/identity}}
        db*    (fn [] (d/empty-db schema))]

    (testing "nested map with string tempids"
      (let [tx (d/with (db*) [{:db/id "e1" :name "Ivan" :profile {:db/id "e2" :email "@2"}}])
            e1 (get (:tempids tx) "e1")
            db (:db-after tx)]
        (is (= "Ivan" (-> (d/entity db e1) :name)))
        (is (= "@2" (-> (d/entity db e1) :profile :email)))))

    (testing "nested map with auto-generated IDs"
      (let [db (d/db-with (db*) [{:name "Ivan" :profile {:email "@2"}}])]
        ;; Verify relationship via query
        (is (= #{["Ivan" "@2"]}
              (d/q '[:find ?name ?email
                     :where [?e :name ?name]
                     [?e :profile ?p]
                     [?p :email ?email]] db)))))

    (testing "issue-59 - nested with no other attrs"
      (let [db (d/db-with (db*) [{:profile {:email "@2"}}])]
        (is (= #{["@2"]}
              (d/q '[:find ?email
                     :where [?e :profile ?p]
                     [?p :email ?email]] db)))))

    (testing "reverse ref"
      (let [db (d/db-with (db*) [{:email "@2" :_profile {:name "Ivan"}}])]
        (is (= #{["Ivan" "@2"]}
              (d/q '[:find ?name ?email
                     :where [?e :name ?name]
                     [?e :profile ?p]
                     [?p :email ?email]] db))))))

  (testing "multi-valued"
    (let [schema {:profile {:db/valueType :db.type/ref
                            :db/cardinality :db.cardinality/many}
                  :name {:db/unique :db.unique/identity}
                  :email {:db/unique :db.unique/identity}}
          db*    (fn [] (d/empty-db schema))]

      (testing "multi-valued with explicit UUIDs"
        (let [e1 (random-uuid)
              e2 (random-uuid)
              db (d/db-with (db*) [{:db/id e1 :name "Ivan" :profile {:db/id e2 :email "@2"}}])]
          (is (= #{["@2"]}
                (d/q '[:find ?email
                       :where [?e :name "Ivan"]
                       [?e :profile ?p]
                       [?p :email ?email]] db)))))

      (testing "multi-valued with multiple profiles"
        (let [e1 (random-uuid)
              e2 (random-uuid)
              e3 (random-uuid)
              db (d/db-with (db*) [{:db/id e1 :name "Ivan"
                                    :profile [{:db/id e2 :email "@2"} {:db/id e3 :email "@3"}]}])]
          (is (= #{["@2"] ["@3"]}
                (d/q '[:find ?email
                       :where [?e :name "Ivan"]
                       [?e :profile ?p]
                       [?p :email ?email]] db)))))

      (testing "multi-valued auto-generated IDs"
        (let [db (d/db-with (db*) [{:name "Ivan" :profile {:email "@2"}}])]
          (is (= #{["@2"]}
                (d/q '[:find ?email
                       :where [?e :name "Ivan"]
                       [?e :profile ?p]
                       [?p :email ?email]] db)))))

      (testing "multi-valued list of profiles"
        (let [db (d/db-with (db*) [{:name "Ivan" :profile [{:email "@2"} {:email "@3"}]}])]
          (is (= #{["@2"] ["@3"]}
                (d/q '[:find ?email
                       :where [?e :name "Ivan"]
                       [?e :profile ?p]
                       [?p :email ?email]] db)))))

      ;; issue-467
      (testing "multi-valued set of profiles"
        (let [db (d/db-with (db*) [{:name "Ivan" :profile #{{:email "@2"} {:email "@3"}}}])]
          (is (= #{["@2"] ["@3"]}
                (d/q '[:find ?email
                       :where [?e :name "Ivan"]
                       [?e :profile ?p]
                       [?p :email ?email]] db)))))

      (testing "multi-valued reverse ref"
        (let [db (d/db-with (db*) [{:email "@2" :_profile {:name "Ivan"}}])]
          (is (= #{["Ivan" "@2"]}
                (d/q '[:find ?name ?email
                       :where [?e :name ?name]
                       [?e :profile ?p]
                       [?p :email ?email]] db)))))

      (testing "multi-valued reverse ref list"
        (let [db (d/db-with (db*) [{:email "@2" :_profile [{:name "Ivan"} {:name "Petr"}]}])]
          (is (= #{["Ivan" "@2"] ["Petr" "@2"]}
                (d/q '[:find ?name ?email
                       :where [?e :name ?name]
                       [?e :profile ?p]
                       [?p :email ?email]] db))))))))

(deftest test-circular-refs
  ;; Test that adding component refs to existing entity works
  (let [schema {:comp {:db/valueType   :db.type/ref
                       :db/cardinality :db.cardinality/many
                       :db/isComponent true}
                :name {:db/unique :db.unique/identity}}
        tx1    (d/with (d/empty-db schema) [{:db/id "e1" :name "Name"}])
        e1     (get (:tempids tx1) "e1")
        db     (d/db-with (:db-after tx1) [{:db/id e1, :comp [{:name "C"}]}])]
    ;; Verify relationships
    (is (= #{["Name" "C"]}
          (d/q '[:find ?parent ?child
                 :where [?e :name ?parent]
                 [?e :comp ?c]
                 [?c :name ?child]] db))))

  (let [schema {:comp {:db/valueType   :db.type/ref
                       :db/cardinality :db.cardinality/many}
                :name {:db/unique :db.unique/identity}}
        tx1    (d/with (d/empty-db schema) [{:db/id "e1" :name "Name"}])
        e1     (get (:tempids tx1) "e1")
        db     (d/db-with (:db-after tx1) [{:db/id e1, :comp [{:name "C"}]}])]
    (is (= #{["Name" "C"]}
          (d/q '[:find ?parent ?child
                 :where [?e :name ?parent]
                 [?e :comp ?c]
                 [?c :name ?child]] db))))

  (let [schema {:comp {:db/valueType   :db.type/ref
                       :db/isComponent true}
                :name {:db/unique :db.unique/identity}}
        tx1    (d/with (d/empty-db schema) [{:db/id "e1" :name "Name"}])
        e1     (get (:tempids tx1) "e1")
        db     (d/db-with (:db-after tx1) [{:db/id e1, :comp {:name "C"}}])]
    (is (= #{["Name" "C"]}
          (d/q '[:find ?parent ?child
                 :where [?e :name ?parent]
                 [?e :comp ?c]
                 [?c :name ?child]] db))))

  (let [schema {:comp {:db/valueType   :db.type/ref}
                :name {:db/unique :db.unique/identity}}
        tx1    (d/with (d/empty-db schema) [{:db/id "e1" :name "Name"}])
        e1     (get (:tempids tx1) "e1")
        db     (d/db-with (:db-after tx1) [{:db/id e1, :comp {:name "C"}}])]
    (is (= #{["Name" "C"]}
          (d/q '[:find ?parent ?child
                 :where [?e :name ?parent]
                 [?e :comp ?c]
                 [?c :name ?child]] db)))))

