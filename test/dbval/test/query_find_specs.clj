(ns dbval.test.query-find-specs
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(def *test-db
  (delay
    (let [tx (d/with (d/empty-db)
               [[:db/add "e1" :name "Petr"]
                [:db/add "e1" :age 44]
                [:db/add "e2" :name "Ivan"]
                [:db/add "e2" :age 25]
                [:db/add "e3" :name "Sergey"]
                [:db/add "e3" :age 11]])]
      {:db (:db-after tx)
       :e1 (get (:tempids tx) "e1")
       :e2 (get (:tempids tx) "e2")
       :e3 (get (:tempids tx) "e3")})))

(deftest test-find-specs
  (let [{:keys [db e1]} @*test-db]
    (is (= (set (d/q '[:find [?name ...]
                       :where [_ :name ?name]] db))
          #{"Ivan" "Petr" "Sergey"}))
    (is (= (d/q '[:find [?name ?age]
                  :in $ ?e
                  :where [?e :name ?name]
                  [?e :age  ?age]] db e1)
          ["Petr" 44]))
    (is (= (d/q '[:find ?name .
                  :in $ ?e
                  :where [?e :name ?name]] db e1)
          "Petr"))

    (testing "Multiple results get cut"
      (is (contains?
            #{["Petr" 44] ["Ivan" 25] ["Sergey" 11]}
            (d/q '[:find [?name ?age]
                   :where [?e :name ?name]
                   [?e :age  ?age]] db)))
      (is (contains?
            #{"Ivan" "Petr" "Sergey"}
            (d/q '[:find ?name .
                   :where [_ :name ?name]] db))))

    (testing "Aggregates work with find specs"
      (is (= (d/q '[:find [(count ?name) ...]
                    :where [_ :name ?name]] db)
            [3]))
      (is (= (d/q '[:find [(count ?name)]
                    :where [_ :name ?name]] db)
            [3]))
      (is (= (d/q '[:find (count ?name) .
                    :where [_ :name ?name]] db)
            3)))))
