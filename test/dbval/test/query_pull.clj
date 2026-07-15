(ns dbval.test.query-pull
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(def *test-db
  (delay
    (let [tx (d/with (d/empty-db)
               [{:db/id "petr" :name "Petr" :age 44}
                {:db/id "ivan" :name "Ivan" :age 25}
                {:db/id "oleg" :name "Oleg" :age 11}])]
      {:db (:db-after tx)
       :petr (get (:tempids tx) "petr")
       :ivan (get (:tempids tx) "ivan")
       :oleg (get (:tempids tx) "oleg")})))

(deftest test-basics
  (let [{:keys [db petr ivan]} @*test-db]
    (are [find res] (= (set (d/q {:find find
                                  :where '[[?e :age ?a]
                                           [(>= ?a 18)]]}
                              db))
                      res)
      '[(pull ?e [:name])]
      #{[{:name "Ivan"}] [{:name "Petr"}]}

      '[(pull ?e [*])]
      #{[{:db/id ivan :age 25 :name "Ivan"}] [{:db/id petr :age 44 :name "Petr"}]}

      '[?e (pull ?e [:name])]
      #{[ivan {:name "Ivan"}] [petr {:name "Petr"}]}

      '[?e ?a (pull ?e [:name])]
      #{[ivan 25 {:name "Ivan"}] [petr 44 {:name "Petr"}]}

      '[?e (pull ?e [:name]) ?a]
      #{[ivan {:name "Ivan"} 25] [petr {:name "Petr"} 44]})))

(deftest test-var-pattern
  (let [{:keys [db petr ivan]} @*test-db]
    (are [find pattern res] (= (set (d/q {:find find
                                          :in   '[$ ?pattern]
                                          :where '[[?e :age ?a]
                                                   [(>= ?a 18)]]}
                                      db pattern))
                              res)
      '[(pull ?e ?pattern)] [:name]
      #{[{:name "Ivan"}] [{:name "Petr"}]}

      '[?e ?a ?pattern (pull ?e ?pattern)] [:name]
      #{[ivan 25 [:name] {:name "Ivan"}] [petr 44 [:name] {:name "Petr"}]})))

;; not supported
#_(deftest test-multi-pattern
    (is (= (set (d/q '[:find ?e ?p (pull ?e ?p)
                       :in $ [?p ...]
                       :where [?e :age ?a]
                       [>= ?a 18]]
                  @*test-db [[:name] [:age]]))
          #{[2 [:name] {:name "Ivan"}]
            [2 [:age]  {:age 25}]
            [1 [:name] {:name "Petr"}]
            [1 [:age]  {:age 44}]})))

(deftest test-multiple-sources
  (let [tx1 (d/with (d/empty-db) [{:db/id "ivan" :name "Ivan" :age 25}])
        db1 (:db-after tx1)
        ivan1 (get (:tempids tx1) "ivan")
        tx2 (d/with (d/empty-db) [{:db/id "petr" :name "Petr" :age 25}])
        db2 (:db-after tx2)
        petr2 (get (:tempids tx2) "petr")]
    (is (= (set (d/q '[:find ?e (pull $1 ?e [:name])
                       :in $1 $2
                       :where [$1 ?e :age 25]]
                  db1 db2))
          #{[ivan1 {:name "Ivan"}]}))

    (is (= (set (d/q '[:find ?e (pull $2 ?e [:name])
                       :in $1 $2
                       :where [$2 ?e :age 25]]
                  db1 db2))
          #{[petr2 {:name "Petr"}]}))

    (testing "$ is default source"
      (is (= (set (d/q '[:find ?e (pull ?e [:name])
                         :in $1 $
                         :where [$ ?e :age 25]]
                    db1 db2))
            #{[petr2 {:name "Petr"}]})))))

(deftest test-find-spec
  (let [{:keys [db ivan]} @*test-db]
    (is (= (d/q '[:find (pull ?e [:name]) .
                  :where [?e :age 25]]
             db)
          {:name "Ivan"}))

    (is (= (set (d/q '[:find [(pull ?e [:name]) ...]
                       :where [?e :age ?a]]
                  db))
          #{{:name "Ivan"} {:name "Petr"} {:name "Oleg"}}))

    (is (= (d/q '[:find [?e (pull ?e [:name])]
                  :where [?e :age 25]]
             db)
          [ivan {:name "Ivan"}]))))

(deftest test-find-spec-input
  (let [{:keys [db ivan]} @*test-db]
    (is (= (d/q '[:find (pull ?e ?p) .
                  :in $ ?p ?eid
                  :where [(ground ?eid) ?e]]
             db [:name] ivan)
          {:name "Ivan"}))
    (is (= (d/q '[:find (pull ?e p) .
                  :in $ p ?eid
                  :where [(ground ?eid) ?e]]
             db [:name] ivan)
          {:name "Ivan"}))))

(deftest test-aggregates
  (let [tx (d/with (d/empty-db {:value {:db/cardinality :db.cardinality/many}})
             [{:db/id "petr" :name "Petr" :value [10 20 30 40]}
              {:db/id "ivan" :name "Ivan" :value [14 16]}
              {:db/id "oleg" :name "Oleg" :value 1}])
        db (:db-after tx)
        petr (get (:tempids tx) "petr")
        ivan (get (:tempids tx) "ivan")
        oleg (get (:tempids tx) "oleg")]
    (is (= (set (d/q '[:find ?e (pull ?e [:name]) (min ?v) (max ?v)
                       :where [?e :value ?v]]
                  db))
          #{[petr {:name "Petr"} 10 40]
            [ivan {:name "Ivan"} 14 16]
            [oleg {:name "Oleg"} 1 1]}))))

(deftest test-lookup-refs
  (let [tx (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
             [{:db/id "petr" :name "Petr" :age 44}
              {:db/id "ivan" :name "Ivan" :age 25}
              {:db/id "oleg" :name "Oleg" :age 11}])
        db (:db-after tx)
        petr (get (:tempids tx) "petr")
        ivan (get (:tempids tx) "ivan")]
    (is (= (set (d/q '[:find ?ref ?a (pull ?ref [:db/id :name])
                       :in   $ [?ref ...]
                       :where [?ref :age ?a]
                       [(>= ?a 18)]]
                  db [[:name "Ivan"] [:name "Oleg"] [:name "Petr"]]))
          #{[[:name "Petr"] 44 {:db/id petr :name "Petr"}]
            [[:name "Ivan"] 25 {:db/id ivan :name "Ivan"}]}))))
