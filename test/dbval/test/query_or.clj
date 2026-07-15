(ns dbval.test.query-or
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc])
  (:import
       [clojure.lang ExceptionInfo]))

;; Use string tempids and capture the mapping
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

(deftest test-or
  (let [{:keys [db e1 e2 e3 e4 e5 e6]} @*test-db]
    ;; intersecting results
    (is (= (d/q '[:find ?e :where (or [?e :name "Oleg"] [?e :age 10])] db)
          #{[e1] [e3] [e4] [e5]}))

    ;; one branch empty
    (is (= (d/q '[:find ?e :where (or [?e :name "Oleg"] [?e :age 30])] db)
          #{[e3] [e4]}))

    ;; both empty
    (is (= (d/q '[:find ?e :where (or [?e :name "Petr"] [?e :age 30])] db)
          #{}))

    ;; join with 1 var
    (is (= (d/q '[:find ?e :where [?e :name "Ivan"] (or [?e :name "Oleg"] [?e :age 10])] db)
          #{[e1] [e5]}))

    ;; join with 2 vars - use :in to bind specific entity IDs
    (is (= (d/q '[:find ?e :in $ ?ref1 ?ref2 :where [?e :age ?a]
                  (or (and [?e :name "Ivan"] [?ref1 :age ?a])
                    (and [?e :name "Oleg"] [?ref2 :age ?a]))]
             db e1 e2)
          #{[e1] [e5] [e4]}))

    ;; OR introduces vars - use :in to bind specific entity IDs
    (is (= (d/q '[:find ?e :in $ ?ref1 ?ref2 :where
                  (or (and [?e :name "Ivan"] [?ref1 :age ?a])
                    (and [?e :name "Oleg"] [?ref2 :age ?a]))
                  [?e :age ?a]]
             db e1 e2)
          #{[e1] [e5] [e4]}))

    ;; OR introduces vars in different order - use :in to bind specific entity IDs
    (is (= (d/q '[:find ?e :in $ ?ref1 ?ref2 :where
                  (or (and [?e :name "Ivan"] [?ref1 :age ?a])
                    (and [?ref2 :age ?a] [?e :name "Oleg"]))
                  [?e :age ?a]]
             db e1 e2)
          #{[e1] [e5] [e4]}))

    ;; One branch of or short-circuits resolution
    (is (= (d/q '[:find ?e :where
                  (or (and [?e :age 30] [?e :name ?n])
                    (and [?e :age 20] [?e :name ?n]))
                  [(ground "Ivan") ?n]]
             db)
          #{[e2] [e6]}))))

(deftest test-or-join
  (let [{:keys [db e1 e2 e3 e4 e5 e6]} @*test-db]
    (is (= (d/q '[:find ?e :where (or-join [?e] [?e :name ?n] (and [?e :age ?a] [?e :name ?n]))] db)
          #{[e1] [e2] [e3] [e4] [e5] [e6]}))

    (is (= (d/q '[:find ?e :where [?e :name ?a] [?e2 :name ?a] (or-join [?e] (and [?e :age ?a] [?e2 :age ?a]))] db)
          #{[e1] [e2] [e3] [e4] [e5] [e6]}))

    ;; One branch of or-join short-circuits resolution
    (is (= (d/q '[:find ?e :where
                  (or-join [?e ?n]
                    (and [?e :age 30] [?e :name ?n])
                    (and [?e :age 20] [?e :name ?n]))
                  [(ground "Ivan") ?n]]
             db)
          #{[e2] [e6]})))

  ;; issue-348 tests - use fresh db each time since they need specific entity refs
  (let [{:keys [db e1 e3 e4 e5]} @*test-db]
    (is (= (d/q '[:find ?e :in $ ?a :where (or [?e :age ?a] [?e :name "Oleg"])]
             db 10)
          #{[e1] [e3] [e4] [e5]}))

    (is (= (d/q '[:find ?e :in $ ?a :where (or-join [?e ?a] [?e :age ?a] [?e :name "Oleg"])]
             db 10)
          #{[e1] [e3] [e4] [e5]}))

    (is (= (d/q '[:find ?e :in $ ?a :where (or-join [[?a] ?e] [?e :age ?a] [?e :name "Oleg"])]
             db 10)
          #{[e1] [e3] [e4] [e5]})))

  (is (= #{[:a1 :b1 :c1]
           [:a2 :b2 :c2]}
        (d/q '[:find ?a ?b ?c
               :in $xs $ys
               :where [$xs ?a ?b ?c]
               (or-join [?a]
                 [$ys ?a ?b ?d])]
          [[:a1 :b1 :c1]
           [:a2 :b2 :c2]
           [:a3 :b3 :c3]]
          [[:a1 :b1  :d1]
           [:a2 :b2* :d2]
           [:a4 :b4 :c4]])))

  (is (= #{[:a1 :c1] [:a2 :c2]}
        (d/q '[:find ?a ?c
               :in $xs $ys
               :where (or-join [?a ?c]
                        [$xs ?a ?b ?c]
                        [$ys ?a ?c])]
          [[:a1 :b1 :c1]]
          [[:a2 :c2]]))))

(deftest test-default-source
  (let [tx1 (d/with (d/empty-db)
              [{:db/id "e1" :name "Ivan"}
               {:db/id "e2" :name "Oleg"}])
        db1 (:db-after tx1)
        e1 (get (:tempids tx1) "e1")
        tx2 (d/with (d/empty-db)
              [{:db/id e1 :age 10}   ;; use same UUID as e1 from db1
               {:db/id "e2b" :age 20}])
        db2 (:db-after tx2)]
    ;; OR inherits default source
    (is (= (d/q '[:find ?e :in $ $2 :where [?e :name] (or [?e :name "Ivan"])] db1 db2)
          #{[e1]}))

    ;; OR can reference any source
    (is (= (d/q '[:find ?e :in $ $2 :where [?e :name] (or [$2 ?e :age 10])] db1 db2)
          #{[e1]}))

    ;; OR can change default source
    (is (= (d/q '[:find ?e :in $ $2 :where [?e :name] ($2 or [?e :age 10])] db1 db2)
          #{[e1]}))

    ;; even with another default source, it can reference any other source explicitly
    (is (= (d/q '[:find ?e :in $ $2 :where [?e :name] ($2 or [$ ?e :name "Ivan"])] db1 db2)
          #{[e1]}))

    ;; nested OR keeps the default source
    (is (= (d/q '[:find ?e :in $ $2 :where [?e :name] ($2 or (or [?e :age 10]))] db1 db2)
          #{[e1]}))

    ;; can override nested OR source
    (is (= (d/q '[:find ?e :in $ $2 :where [?e :name] ($2 or ($ or [?e :name "Ivan"]))] db1 db2)
          #{[e1]}))))

(deftest ^{:doc "issue-468, issue-469"} test-const-substitution
  (let [tx (d/with (d/empty-db {:parent {:db/valueType :db.type/ref}})
             [{:db/id "Ivan" :name "Ivan"}
              {:db/id "Oleg" :name "Oleg" :parent "Ivan"}
              {:db/id "Petr" :name "Petr" :parent "Oleg"}])
        db (:db-after tx)
        ivan (get (:tempids tx) "Ivan")
        oleg (get (:tempids tx) "Oleg")
        _petr (get (:tempids tx) "Petr")]
    ;; Query: find ?x named "Ivan" (= ivan), and ?y where either:
    ;; 1. ?x has parent ?z, and ?z has parent ?y (Ivan has no parent, so fails)
    ;; 2. ?y has parent ?x (Oleg has parent Ivan, so ?y = oleg)
    (is (= #{["Ivan" ivan oleg]}
          (d/q '[:find ?name ?x ?y
                 :in $ ?name
                 :where
                 [?x :name ?name]
                 (or-join [?x ?y]
                   (and
                     [?x :parent ?z]
                     [?z :parent ?y])
                   [?y :parent ?x])]
            db "Ivan")))

    (is (= #{}
          (d/q '[:find ?name ?x ?y
                 :in $ ?name
                 :where
                 [?x :name ?name]
                 (or-join [?x ?y]
                   (and
                     [?x :parent ?z]
                     [?z :parent ?y])
                   [?x :parent ?y])]
            db "Ivan")))))

(deftest test-errors
  (let [{:keys [db]} @*test-db]
    (is (thrown-with-msg? ExceptionInfo #"All clauses in 'or' must use same set of free vars, had \[#\{\?e\} #\{(\?a \?e|\?e \?a)\}\] in \(or \[\?e :name _\] \[\?e :age \?a\]\)"
          (d/q '[:find ?e
                 :where (or [?e :name _]
                          [?e :age ?a])]
            db)))

    (is (thrown-msg? "Insufficient bindings: #{?e} not bound in (or-join [[?e]] [?e :name \"Ivan\"])"
          (d/q '[:find ?e
                 :where (or-join [[?e]]
                          [?e :name "Ivan"])]
            db)))))
