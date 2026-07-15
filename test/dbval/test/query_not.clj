(ns dbval.test.query-not
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc])
  (:import
       [clojure.lang ExceptionInfo]))

(def *test-db
  (delay
    (let [tx (d/with (d/empty-db)
               [{:db/id "e1" :name "Ivan" :age 10}
                {:db/id "e2" :name "Ivan" :age 20}
                {:db/id "e3" :name "Oleg" :age 10}
                {:db/id "e4" :name "Oleg" :age 20}
                {:db/id "e5" :name "Ivan" :age 10}
                {:db/id "e6" :name "Ivan" :age 20}])
          tempids (:tempids tx)]
      {:db (:db-after tx)
       :e1 (get tempids "e1")
       :e2 (get tempids "e2")
       :e3 (get tempids "e3")
       :e4 (get tempids "e4")
       :e5 (get tempids "e5")
       :e6 (get tempids "e6")})))

(deftest test-not
  (let [{:keys [db e1 e2 e3 e4 e5 e6]} @*test-db]
    (is (= (set (d/q '[:find [?e ...] :where [?e :name] (not [?e :name "Ivan"])] db))
          #{e3 e4}))

    (is (= (set (d/q '[:find [?e ...] :where [?e :name] (not [?e :name "Ivan"] [?e :age 10])] db))
          #{e2 e3 e4 e6}))

    (is (= (set (d/q '[:find [?e ...] :where [?e :name] (not [?e :name "Ivan"]) (not [?e :age 10])] db))
          #{e4}))

    ;; full exclude
    (is (= (set (d/q '[:find [?e ...] :where [?e :name] (not [?e :age])] db))
          #{}))

    ;; not-intersecting rels
    (is (= (set (d/q '[:find [?e ...] :where [?e :name "Ivan"] (not [?e :name "Oleg"])] db))
          #{e1 e2 e5 e6}))

    ;; exclude empty set
    (is (= (set (d/q '[:find [?e ...] :where [?e :name] (not [?e :name "Ivan"] [?e :name "Oleg"])] db))
          #{e1 e2 e3 e4 e5 e6}))

    ;; nested excludes
    (is (= (set (d/q '[:find [?e ...] :where [?e :name] (not [?e :name "Ivan"] (not [?e :age 10]))] db))
          #{e1 e3 e4 e5}))

    ;; extra binding in not
    (is (= (set (d/q '[:find [?e ...] :where [?e :name ?a] (not [?e :age ?f] [?e :age 10])] db))
          #{e2 e4 e6}))))

(deftest test-not-join
  (let [{:keys [db e1 e2 e3 e4 e5 e6]} @*test-db]
    (is (= (d/q '[:find ?e ?a :where [?e :name] [?e :age ?a] (not-join [?e] [?e :name "Oleg"] [?e :age ?a])] db)
          #{[e1 10] [e2 20] [e5 10] [e6 20]}))

    (is (= (d/q '[:find ?e ?a :where [?e :age ?a] [?e :age 10] (not-join [?e] [?e :name "Oleg"] [?e :age ?a] [?e :age 10])] db)
          #{[e1 10] [e5 10]}))

    ;; issue-481
    (is (= (d/q '[:find ?e ?a :where [?e :age ?a] (not-join [?a] [?e :name "Petr"] [?e :age ?a])] db)
          #{[e1 10] [e2 20] [e3 10] [e4 20] [e5 10] [e6 20]}))))

(deftest test-default-source
  (let [tx1 (d/with (d/empty-db)
              [{:db/id "e1" :name "Ivan"}
               {:db/id "e2" :name "Oleg"}])
        db1 (:db-after tx1)
        e1 (get (:tempids tx1) "e1")
        e2 (get (:tempids tx1) "e2")
        tx2 (d/with (d/empty-db)
              [{:db/id e1 :age 10}
               {:db/id e2 :age 20}])
        db2 (:db-after tx2)]
    ;; NOT inherits default source
    (is (= (set (d/q '[:find [?e ...] :in $ $2 :where [?e :name] (not [?e :name "Ivan"])] db1 db2))
          #{e2}))

    ;; NOT can reference any source
    (is (= (set (d/q '[:find [?e ...] :in $ $2 :where [?e :name] (not [$2 ?e :age 10])] db1 db2))
          #{e2}))

    ;; NOT can change default source
    (is (= (set (d/q '[:find [?e ...] :in $ $2 :where [?e :name] ($2 not [?e :age 10])] db1 db2))
          #{e2}))

    ;; even with another default source, it can reference any other source explicitly
    (is (= (set (d/q '[:find [?e ...] :in $ $2 :where [?e :name] ($2 not [$ ?e :name "Ivan"])] db1 db2))
          #{e2}))

    ;; nested NOT keeps the default source
    (is (= (set (d/q '[:find [?e ...] :in $ $2 :where [?e :name] ($2 not (not [?e :age 10]))] db1 db2))
          #{e1}))

    ;; can override nested NOT source
    (is (= (set (d/q '[:find [?e ...] :in $ $2 :where [?e :name] ($2 not ($ not [?e :name "Ivan"]))] db1 db2))
          #{e1}))))

(deftest test-impl-edge-cases
  (let [{:keys [db e1 e2 e3 e4 e5 e6]} @*test-db]
    ;; const \ empty
    (is (= (d/q '[:find ?e :where [?e :name "Oleg"] [?e :age 10] (not [?e :age 20])] db)
          #{[e3]}))

    ;; const \ const
    (is (= (d/q '[:find ?e :where [?e :name "Oleg"] [?e :age 10] (not [?e :age 10])] db)
          #{}))

    ;; rel \ const
    (is (= (d/q '[:find ?e :where [?e :name "Oleg"] (not [?e :age 10])] db)
          #{[e4]}))

    ;; 2 rels \ 2 rels
    (is (= (d/q '[:find ?e ?e2 :where [?e :name "Ivan"] [?e2 :name "Ivan"]
                  (not [?e :age 10] [?e2 :age 20])] db)
          #{[e2 e1] [e6 e5] [e1 e1] [e2 e2] [e5 e5] [e6 e6]
            [e2 e5] [e1 e5] [e2 e6] [e6 e1] [e5 e1] [e6 e2]}))

    ;; 2 rels \ rel + const
    (is (= (d/q '[:find ?e ?e2 :where [?e :name "Ivan"] [?e2 :name "Oleg"]
                  (not [?e :age 10] [?e2 :age 20])] db)
          #{[e2 e3] [e1 e3] [e2 e4] [e6 e3] [e5 e3] [e6 e4]}))

    ;; 2 rels \ 2 consts
    (is (= (d/q '[:find ?e ?e2 :where [?e :name "Oleg"] [?e2 :name "Oleg"]
                  (not [?e :age 10] [?e2 :age 20])] db)
          #{[e4 e3] [e3 e3] [e4 e4]}))))

(deftest test-insufficient-bindings
  (let [{:keys [db e1]} @*test-db]
    (is (thrown-msg? "Insufficient bindings: none of #{?e} is bound in (not [?e :name \"Ivan\"])"
          (d/q '[:find ?e :where (not [?e :name "Ivan"]) [?e :name]] db)))

    (is (thrown-msg? "Insufficient bindings: none of #{?a ?ref} is bound in (not [?ref :age ?a])"
          (d/q '[:find ?e :in $ ?ref :where [?e :name] (not-join [?e] (not [?ref :age ?a]) [?e :age ?a])] db e1)))

    (is (thrown-msg? "Insufficient bindings: none of #{?a} is bound in (not [?a :name \"Ivan\"])"
          (d/q '[:find ?e :where [?e :name] (not [?a :name "Ivan"])] db)))))
