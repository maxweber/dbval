(ns dbval.test.query-return-map
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(def *test-db
  (delay
    (d/db-with (d/empty-db)
      [[:db/add "e1" :name "Petr"]
       [:db/add "e1" :age 44]
       [:db/add "e2" :name "Ivan"]
       [:db/add "e2" :age 25]
       [:db/add "e3" :name "Sergey"]
       [:db/add "e3" :age 11]])))

(deftest test-find-specs
  (is (= (d/q '[:find ?name ?age
                :keys n a
                :where [?e :name ?name]
                [?e :age  ?age]]
           @*test-db)
        #{{:n "Petr" :a 44} {:n "Ivan" :a 25} {:n "Sergey" :a 11}}))
  (is (= (d/q '[:find ?name ?age
                :syms n a
                :where [?e :name ?name]
                [?e :age  ?age]]
           @*test-db)
        #{{'n "Petr" 'a 44} {'n "Ivan" 'a 25} {'n "Sergey" 'a 11}}))
  (is (= (d/q '[:find ?name ?age
                :strs n a
                :where [?e :name ?name]
                [?e :age  ?age]]
           @*test-db)
        #{{"n" "Petr" "a" 44} {"n" "Ivan" "a" 25} {"n" "Sergey" "a" 11}}))

  (is (= (d/q '[:find [?name ?age]
                :keys n a
                :where [?e :name ?name]
                [(= ?name "Ivan")]
                [?e :age  ?age]]
           @*test-db)
        {:n "Ivan" :a 25})))

