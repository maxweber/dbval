(ns dbval.test.tuples
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc])
  (:import
       [clojure.lang ExceptionInfo]))

(deftest test-schema
  (let [db (d/empty-db
             {:year+session {:db/tupleAttrs [:year :session]}
              :semester+course+student {:db/tupleAttrs [:semester :course :student]}
              :session+student {:db/tupleAttrs [:session :student]
                                :db/valueType :db.type/tuple}})]
    (is (= #{:year+session :semester+course+student :session+student}
          (db/-attrs-by db :db.type/tuple)))

    (is (= {:year     {:year+session 0}
            :session  {:year+session 1, :session+student 0}
            :semester {:semester+course+student 0}
            :course   {:semester+course+student 1}
            :student  {:semester+course+student 2, :session+student 1}}
          (db/-attrs-by db :db/attrTuples)))

    (is (thrown-msg? ":t2 :db/tupleAttrs can't depend on another tuple attribute: :t1"
          (d/empty-db {:t1 {:db/tupleAttrs [:a :b]}
                       :t2 {:db/tupleAttrs [:c :d :e :t1]}})))

    (is (thrown-msg? ":t1 :db/tupleAttrs must be a sequential collection, got: :a"
          (d/empty-db {:t1 {:db/tupleAttrs :a}})))

    (is (thrown-msg? ":t1 :db/tupleAttrs can't be empty"
          (d/empty-db {:t1 {:db/tupleAttrs ()}})))

    (is (thrown-msg? ":t1 has :db/tupleAttrs, must be :db.cardinality/one"
          (d/empty-db {:t1 {:db/tupleAttrs [:a :b :c]
                            :db/cardinality :db.cardinality/many}})))

    (is (thrown-msg? ":t1 :db/tupleAttrs can't depend on :db.cardinality/many attribute: :a"
          (d/empty-db {:a  {:db/cardinality :db.cardinality/many}
                       :t1 {:db/tupleAttrs [:a :b :c]}}))))
  (is (thrown-msg? "Bad attribute specification for :foo+bar: {:db/valueType :db.type/tuple} should also have :db/tupleAttrs"
        (d/empty-db {:foo+bar {:db/valueType :db.type/tuple}}))))

(deftest test-tx
  ;; Test tuple attribute auto-population
  (let [schema {:a+b   {:db/tupleAttrs [:a :b]}
                :a+c+d {:db/tupleAttrs [:a :c :d]}}]
    ;; Create entity and track its ID
    (let [tx1 (d/with (d/empty-db schema) [{:db/id "e1" :a "a"}])
          e1 (get (:tempids tx1) "e1")]
      (is (= #{[:a "a"] [:a+b ["a" nil]] [:a+c+d ["a" nil nil]]}
            (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx1) :eavt e1)))))

      ;; Add :b to the entity
      (let [tx2 (d/with (:db-after tx1) [[:db/add e1 :b "b"]])]
        (is (= #{[:a "a"] [:b "b"] [:a+b ["a" "b"]] [:a+c+d ["a" nil nil]]}
              (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx2) :eavt e1)))))

        ;; Update :a
        (let [tx3 (d/with (:db-after tx2) [[:db/add e1 :a "A"]])]
          (is (= #{[:a "A"] [:b "b"] [:a+b ["A" "b"]] [:a+c+d ["A" nil nil]]}
                (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx3) :eavt e1)))))

          ;; Add :c and :d
          (let [tx4 (d/with (:db-after tx3) [[:db/add e1 :c "c"] [:db/add e1 :d "d"]])]
            (is (= #{[:a "A"] [:b "b"] [:a+b ["A" "b"]] [:c "c"] [:d "d"] [:a+c+d ["A" "c" "d"]]}
                  (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx4) :eavt e1)))))

            ;; Update :a again
            (let [tx5 (d/with (:db-after tx4) [[:db/add e1 :a "a"]])]
              (is (= #{[:a "a"] [:b "b"] [:a+b ["a" "b"]] [:c "c"] [:d "d"] [:a+c+d ["a" "c" "d"]]}
                    (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx5) :eavt e1)))))

              ;; Update all
              (let [tx6 (d/with (:db-after tx5)
                          [[:db/add e1 :a "A"]
                           [:db/add e1 :b "B"]
                           [:db/add e1 :c "C"]
                           [:db/add e1 :d "D"]])]
                (is (= #{[:a "A"] [:b "B"] [:a+b ["A" "B"]] [:c "C"] [:d "D"] [:a+c+d ["A" "C" "D"]]}
                      (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx6) :eavt e1)))))

                ;; Retract :a
                (let [tx7 (d/with (:db-after tx6) [[:db/retract e1 :a "A"]])]
                  (is (= #{[:b "B"] [:a+b [nil "B"]] [:c "C"] [:d "D"] [:a+c+d [nil "C" "D"]]}
                        (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx7) :eavt e1)))))

                  ;; Retract :b
                  (let [tx8 (d/with (:db-after tx7) [[:db/retract e1 :b "B"]])]
                    (is (= #{[:c "C"] [:d "D"] [:a+c+d [nil "C" "D"]]}
                          (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx8) :eavt e1))))))))))))))

  ;; Test that direct tuple modification is rejected
  (let [tx (d/with (d/empty-db {:a+b {:db/tupleAttrs [:a :b]}}) [{:db/id "e1" :a "a"}])
        e1 (get (:tempids tx) "e1")]
    (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
          (d/with (:db-after tx) [{:db/id e1 :a+b ["A" "B"]}])))))

(deftest test-ignore-correct
  (let [conn (d/create-conn {:a+b {:db/tupleAttrs [:a :b]}})]
    (testing "insert"
      (let [tx1 (d/transact! conn [{:db/id "e1" :a "a" :b "b" :a+b ["a" "b"]}])
            e1 (get (:tempids tx1) "e1")]
        (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
              (d/transact! conn [{:db/id "e2" :a "x" :b "y" :a+b ["a" "b"]}])))
        (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
              (d/transact! conn [{:db/id "e2" :a+b ["a" "b"] :a "x" :b "y"}])))
        (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
              (d/transact! conn [{:db/id "e2" :a "a" :b "b" :a+b ["a"]}])))
        (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
              (d/transact! conn [{:db/id "e2" :a "a" :b "b" :a+b ["a" nil]}])))

        (testing "update"
          (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
                (d/transact! conn [{:db/id e1 :a "x" :a+b ["a" "b"]}])))
          (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
                (d/transact! conn [{:db/id e1 :a+b ["a" "B"]}])))
          (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
                (d/transact! conn [{:db/id e1 :a "a" :b "b" :a+b ["a"]}])))
          (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
                (d/transact! conn [{:db/id e1 :a "a" :b "b" :a+b ["a" nil]}])))
          (d/transact! conn [{:db/id e1 :a+b ["a" "b"]}])
          (d/transact! conn [{:db/id e1 :b "B" :a+b ["a" "B"]}])
          (d/transact! conn [{:db/id e1 :a+b ["A" "B"] :a "A"}]))))))

(deftest test-unique
  (let [conn (d/create-conn {:a+b {:db/tupleAttrs [:a :b]
                                   :db/unique :db.unique/identity}})]
    (let [tx1 (d/transact! conn [{:db/id "e1" :a "a"}])
          e1 (get (:tempids tx1) "e1")
          tx2 (d/transact! conn [{:db/id "e2" :a "A"}])
          e2 (get (:tempids tx2) "e2")]
      (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
            (d/transact! conn [[:db/add e1 :a "A"]])))

      (let [tx3 (d/transact! conn [[:db/add e1 :b "b"]
                                    [:db/add e2 :b "b"]
                                    {:db/id "e3" :a "a" :b "B"}])
            e3 (get (:tempids tx3) "e3")]

        (is (= #{[e1 :a "a"]
                 [e1 :b "b"]
                 [e1 :a+b ["a" "b"]]
                 [e2 :a "A"]
                 [e2 :b "b"]
                 [e2 :a+b ["A" "b"]]
                 [e3 :a "a"]
                 [e3 :b "B"]
                 [e3 :a+b ["a" "B"]]}
              (tdc/all-datoms (d/db conn))))

        (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
              (d/transact! conn [[:db/add e1 :a "A"]])))
        (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
              (d/transact! conn [[:db/add e1 :b "B"]])))
        (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
              (d/transact! conn [[:db/add e1 :a "A"]
                                 [:db/add e1 :b "B"]])))

        (testing "multiple tuple updates"
          ;; changing both tuple components in a single operation
          (d/transact! conn [{:db/id e1 :a "A" :b "B"}])
          (is (= {:db/id e1 :a "A" :b "B" :a+b ["A" "B"]}
                (d/pull (d/db conn) '[*] e1)))

          ;; adding entity with two tuple components in a single operation
          (let [tx4 (d/transact! conn [{:db/id "e4" :a "a" :b "b"}])
                e4 (get (:tempids tx4) "e4")]
            (is (= {:db/id e4 :a "a" :b "b" :a+b ["a" "b"]}
                  (d/pull (d/db conn) '[*] e4)))))))))

(deftest test-upsert
  (let [conn (d/create-conn {:a+b {:db/tupleAttrs [:a :b]
                                   :db/unique :db.unique/identity}
                             :c   {:db/unique :db.unique/identity}})]
    (let [tx1 (d/transact! conn
                [{:db/id "e1" :a "A" :b "B"}
                 {:db/id "e2" :a "a" :b "b"}])
          e1 (get (:tempids tx1) "e1")
          e2 (get (:tempids tx1) "e2")]

      (d/transact! conn [{:a+b ["A" "B"] :c "C"}
                         {:a+b ["a" "b"] :c "c"}])
      (is (= #{[e1 :a "A"]
               [e1 :b "B"]
               [e1 :a+b ["A" "B"]]
               [e1 :c "C"]
               [e2 :a "a"]
               [e2 :b "b"]
               [e2 :a+b ["a" "b"]]
               [e2 :c "c"]}
            (tdc/all-datoms (d/db conn))))

      (is (thrown-with-msg? ExceptionInfo #"Conflicting upserts:"
            (d/transact! conn [{:a+b ["A" "B"] :c "c"}])))

      ;; change tuple + upsert
      (d/transact! conn
        [{:a+b ["A" "B"]
          :b "b"
          :d "D"}])

      (is (= #{[e1 :a "A"]
               [e1 :b "b"]
               [e1 :a+b ["A" "b"]]
               [e1 :c "C"]
               [e1 :d "D"]
               [e2 :a "a"]
               [e2 :b "b"]
               [e2 :a+b ["a" "b"]]
               [e2 :c "c"]}
            (tdc/all-datoms (d/db conn)))))))

;; issue-473
(deftest test-upsert-by-tuple-components
  (let [db* (fn []
              (let [tx (d/with
                         (d/empty-db {:a+b {:db/tupleAttrs [:a :b]
                                            :db/unique :db.unique/identity}})
                         [{:db/id "e1" :a "A" :b "B" :name "Ivan"}])]
                {:db (:db-after tx)
                 :e1 (get (:tempids tx) "e1")}))]
    (let [{:keys [db e1]} (db*)]
      (is (= #{[e1 :a "A"]
               [e1 :b "B"]
               [e1 :a+b ["A" "B"]]
               [e1 :name "Oleg"]}
            (tdc/all-datoms
              (d/db-with db
                [{:db/id "new" :a "A" :b "B" :name "Oleg"}])))))
    (let [{:keys [db e1]} (db*)]
      (is (= #{[e1 :a "A"]
               [e1 :b "B"]
               [e1 :a+b ["A" "B"]]
               [e1 :name "Oleg"]}
            (tdc/all-datoms
              (d/db-with db
                [{:a "A" :b "B" :name "Oleg"}])))))
    (let [{:keys [db e1]} (db*)]
      (is (= #{[e1 :a "A"]
               [e1 :b "B"]
               [e1 :a+b ["A" "B"]]
               [e1 :name "Oleg"]}
            (tdc/all-datoms
              (d/db-with db
                [[:db/add "new" :a "A"]
                 [:db/add "new" :b "B"]
                 [:db/add "new" :name "Oleg"]])))))))

(deftest test-lookup-refs
  (let [conn (d/create-conn {:a+b {:db/tupleAttrs [:a :b]
                                   :db/unique :db.unique/identity}
                             :c   {:db/unique :db.unique/identity}})]
    (let [tx1 (d/transact! conn
                [{:db/id "e1" :a "A" :b "B"}
                 {:db/id "e2" :a "a" :b "b"}])
          e1 (get (:tempids tx1) "e1")
          e2 (get (:tempids tx1) "e2")]

      (d/transact! conn [[:db/add [:a+b ["A" "B"]] :c "C"]
                         {:db/id [:a+b ["a" "b"]] :c "c"}])
      (is (= #{[e1 :a "A"]
               [e1 :b "B"]
               [e1 :a+b ["A" "B"]]
               [e1 :c "C"]
               [e2 :a "a"]
               [e2 :b "b"]
               [e2 :a+b ["a" "b"]]
               [e2 :c "c"]}
            (tdc/all-datoms (d/db conn))))

      (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
            (d/transact! conn [[:db/add [:a+b ["A" "B"]] :c "c"]])))

      (is (thrown-with-msg? ExceptionInfo #"Conflicting upsert:"
            (d/transact! conn [{:db/id [:a+b ["A" "B"]] :c "c"}])))

      ;; change tuple + upsert
      (d/transact! conn
        [{:db/id [:a+b ["A" "B"]]
          :b "b"
          :d "D"}])

      (is (= #{[e1 :a "A"]
               [e1 :b "b"]
               [e1 :a+b ["A" "b"]]
               [e1 :c "C"]
               [e1 :d "D"]
               [e2 :a "a"]
               [e2 :b "b"]
               [e2 :a+b ["a" "b"]]
               [e2 :c "c"]}
            (tdc/all-datoms (d/db conn))))

      (is (= {:db/id e2
              :a     "a"
              :b     "b"
              :a+b   ["a" "b"]
              :c     "c"}
            (d/pull (d/db conn) '[*] [:a+b ["a" "b"]]))))))

;; issue-452
(deftest lookup-refs-in-tuple
  (let [schema {:ref      {:db/valueType :db.type/ref}
                :name     {:db/unique :db.unique/identity}
                :ref+name {:db/valueType :db.type/tuple
                           :db/tupleAttrs [:ref :name]
                           :db/unique :db.unique/identity}}
        db* (fn []
              (let [tx (d/with (d/empty-db schema)
                         [{:db/id "ivan" :name "Ivan"}
                          {:db/id "oleg" :name "Oleg"}
                          {:db/id "petr" :name "Petr" :ref "ivan"}
                          {:db/id "yuri" :name "Yuri" :ref "oleg"}])]
                {:db (:db-after tx)
                 :ivan (get (:tempids tx) "ivan")
                 :oleg (get (:tempids tx) "oleg")
                 :petr (get (:tempids tx) "petr")
                 :yuri (get (:tempids tx) "yuri")}))]
    (let [{:keys [db ivan petr]} (db*)]
      (let [db' (d/db-with db [{:ref+name [ivan "Petr"], :age 32}])]
        (is (= {:age 32} (d/pull db' [:age] petr)))))

    (let [{:keys [db petr]} (db*)]
      (let [db' (d/db-with db [{:ref+name [[:name "Ivan"] "Petr"], :age 32}])]
        (is (= {:age 32} (d/pull db' [:age] petr)))))

    (let [{:keys [db ivan petr]} (db*)]
      (is (= ivan (:db/id (d/entity db [:name "Ivan"]))))
      (is (= petr (:db/id (d/entity db [:ref+name [ivan "Petr"]]))))
      (is (= petr (:db/id (d/entity db [:ref+name [[:name "Ivan"] "Petr"]])))))))

(deftest test-validation
  (let [db* (fn [] (d/empty-db {:a+b {:db/tupleAttrs [:a :b]}}))
        db1* (fn []
               (let [tx (d/with (db*) [{:db/id "e1" :a "a"}])]
                 {:db (:db-after tx)
                  :e1 (get (:tempids tx) "e1")}))]
    (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
                     (d/db-with (db*) [{:db/id "e1" :a+b [nil nil]}])))
    (let [{:keys [db e1]} (db1*)]
      (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
                       (d/db-with db [[:db/add e1 :a+b ["a" nil]]]))))
    (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
                     (d/db-with (db*) [{:db/id "e1" :a "a"}
                                       [:db/add "e1" :a+b ["a" nil]]])))
    (let [{:keys [db e1]} (db1*)]
      (is (thrown-with-msg? ExceptionInfo #"Can't modify tuple attrs directly:"
                       (d/db-with db [[:db/retract e1 :a+b ["a" nil]]]))))))

(deftest test-indexes
  (let [tx (d/with (d/empty-db {:a+b+c {:db/tupleAttrs [:a :b :c]}})
             [{:db/id "e1" :a "a" :b "b" :c "c"}
              {:db/id "e2" :a "A" :b "b" :c "c"}
              {:db/id "e3" :a "a" :b "B" :c "c"}
              {:db/id "e4" :a "A" :b "B" :c "c"}
              {:db/id "e5" :a "a" :b "b" :c "C"}
              {:db/id "e6" :a "A" :b "b" :c "C"}
              {:db/id "e7" :a "a" :b "B" :c "C"}
              {:db/id "e8" :a "A" :b "B" :c "C"}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")
        e4 (get (:tempids tx) "e4")
        e5 (get (:tempids tx) "e5")
        e6 (get (:tempids tx) "e6")
        e7 (get (:tempids tx) "e7")
        e8 (get (:tempids tx) "e8")]
    (is (= [e6]
          (mapv :e (d/datoms db :avet :a+b+c ["A" "b" "C"]))))
    (is (= []
          (mapv :e (d/datoms db :avet :a+b+c ["A" "b" nil]))))
    (is (= #{e8 e4 e6 e2}
          (set (mapv :e (d/index-range db :a+b+c ["A" "B" "C"] ["A" "b" "c"])))))
    (is (= #{e8 e4}
          (set (mapv :e (d/index-range db :a+b+c ["A" "B" nil] ["A" "b" nil])))))))

(deftest test-queries
  (let [tx (d/with (d/empty-db {:a+b {:db/tupleAttrs [:a :b]
                                       :db/unique :db.unique/identity}})
             [{:db/id "e1" :a "A" :b "B"}
              {:db/id "e2" :a "A" :b "b"}
              {:db/id "e3" :a "a" :b "B"}
              {:db/id "e4" :a "a" :b "b"}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")
        e4 (get (:tempids tx) "e4")]
    (is (= #{[e3]}
          (d/q '[:find ?e
                 :where [?e :a+b ["a" "B"]]] db)))

    (is (= #{[["a" "B"]]}
          (d/q '[:find ?a+b
                 :where [[:a+b ["a" "B"]] :a+b ?a+b]] db)))

    (is (= #{[["A" "B"]] [["A" "b"]] [["a" "B"]] [["a" "b"]]}
          (d/q '[:find ?a+b
                 :where [?e :a ?a]
                 [?e :b ?b]
                 [(tuple ?a ?b) ?a+b]] db)))

    (is (= #{["A" "B"] ["A" "b"] ["a" "B"] ["a" "b"]}
          (d/q '[:find ?a ?b
                 :where [?e :a+b ?a+b]
                 [(untuple ?a+b) [?a ?b]]] db)))))
