(ns dbval.test.query
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc])
  (:import
       [clojure.lang ExceptionInfo]))

(deftest test-joins
  (let [tx (d/with (d/empty-db)
             [{:db/id "e1", :name  "Ivan", :age   15}
              {:db/id "e2", :name  "Petr", :age   37}
              {:db/id "e3", :name  "Ivan", :age   37}
              {:db/id "e4", :age 15}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]
    (is (= (d/q '[:find ?e
                  :where [?e :name]] db)
          #{[e1] [e2] [e3]}))
    (is (= (d/q '[:find  ?e ?v
                  :where [?e :name "Ivan"]
                  [?e :age ?v]] db)
          #{[e1 15] [e3 37]}))
    (is (= (d/q '[:find  ?e1 ?e2
                  :where [?e1 :name ?n]
                  [?e2 :name ?n]] db)
          #{[e1 e1] [e2 e2] [e3 e3] [e1 e3] [e3 e1]}))
    (is (= (d/q '[:find  ?e ?e2 ?n
                  :where [?e :name "Ivan"]
                  [?e :age ?a]
                  [?e2 :age ?a]
                  [?e2 :name ?n]] db)
          #{[e1 e1 "Ivan"]
            [e3 e3 "Ivan"]
            [e3 e2 "Petr"]}))))

(deftest test-q-many
  (let [tx (d/with (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
             [[:db/add "e1" :name "Ivan"]
              [:db/add "e1" :aka  "ivolga"]
              [:db/add "e1" :aka  "pi"]
              [:db/add "e2" :name "Petr"]
              [:db/add "e2" :aka  "porosenok"]
              [:db/add "e2" :aka  "pi"]])
        db (:db-after tx)]
    (is (= (d/q '[:find  ?n1 ?n2
                  :where [?e1 :aka ?x]
                  [?e2 :aka ?x]
                  [?e1 :name ?n1]
                  [?e2 :name ?n2]] db)
          #{["Ivan" "Ivan"]
            ["Petr" "Petr"]
            ["Ivan" "Petr"]
            ["Petr" "Ivan"]}))))

(deftest test-q-coll
  (let [db [[1 :name "Ivan"]
            [1 :age  19]
            [1 :aka  "dragon_killer_94"]
            [1 :aka  "-=autobot=-"]]]
    (is (= (d/q '[:find  ?n ?a
                  :where [?e :aka "dragon_killer_94"]
                  [?e :name ?n]
                  [?e :age  ?a]] db)
          #{["Ivan" 19]})))

  (testing "Query over long tuples"
    (let [db [[1 :name "Ivan" 945 :db/add]
              [1 :age  39     999 :db/retract]]]
      (is (= (d/q '[:find  ?e ?v
                    :where [?e :name ?v]] db)
            #{[1 "Ivan"]}))
      (is (= (d/q '[:find  ?e ?a ?v ?t
                    :where [?e ?a ?v ?t :db/retract]] db)
            #{[1 :age 39 999]})))))

(deftest test-q-in
  (let [tx (d/with (d/empty-db)
             [{:db/id "e1", :name  "Ivan", :age   15}
              {:db/id "e2", :name  "Petr", :age   37}
              {:db/id "e3", :name  "Ivan", :age   37}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")
        query '{:find  [?e]
                :in    [$ ?attr ?value]
                :where [[?e ?attr ?value]]}]
    (is (= (d/q query db :name "Ivan")
          #{[e1] [e3]}))
    (is (= (d/q query db :age 37)
          #{[e2] [e3]}))

    (testing "Named DB"
      (is (= (d/q '[:find  ?a ?v
                    :in    $db ?e
                    :where [$db ?e ?a ?v]] db e1)
            #{[:name "Ivan"]
              [:age 15]})))

    (testing "DB join with collection"
      (is (= (d/q '[:find  ?e ?email
                    :in    $ $b
                    :where [?e :name ?n]
                    [$b ?n ?email]]
               db
               [["Ivan" "ivan@mail.ru"]
                ["Petr" "petr@gmail.com"]])
            #{[e1 "ivan@mail.ru"]
              [e2 "petr@gmail.com"]
              [e3 "ivan@mail.ru"]})))

    (testing "Query without DB"
      (is (= (d/q '[:find ?a ?b
                    :in   ?a ?b]
               10 20)
            #{[10 20]})))

    (is (thrown-msg? "Extra inputs passed, expected: [], got: 1"
          (d/q '[:find ?e :where [(inc 1) ?e]] db)))

    (is (thrown-msg? "Too few inputs passed, expected: [$ $2], got: 1"
          (d/q '[:find ?e :in $ $2 :where [?e]] db)))

    (is (thrown-msg? "Extra inputs passed, expected: [$], got: 2"
          (d/q '[:find ?e :where [?e]] db db)))

    (is (thrown-msg? "Extra inputs passed, expected: [$ $2], got: 3"
          (d/q '[:find ?e :in $ $2 :where [?e]] db db db)))))

(deftest test-bindings
  (let [tx (d/with (d/empty-db)
             [{:db/id "e1", :name  "Ivan", :age   15}
              {:db/id "e2", :name  "Petr", :age   37}
              {:db/id "e3", :name  "Ivan", :age   37}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]
    (testing "Relation binding"
      (is (= (d/q '[:find  ?e ?email
                    :in    $ [[?n ?email]]
                    :where [?e :name ?n]]
               db
               [["Ivan" "ivan@mail.ru"]
                ["Petr" "petr@gmail.com"]])
            #{[e1 "ivan@mail.ru"]
              [e2 "petr@gmail.com"]
              [e3 "ivan@mail.ru"]})))

    (testing "Tuple binding"
      (is (= (d/q '[:find  ?e
                    :in    $ [?name ?age]
                    :where [?e :name ?name]
                    [?e :age ?age]]
               db ["Ivan" 37])
            #{[e3]})))

    (testing "Collection binding"
      (is (= (d/q '[:find  ?attr ?value
                    :in    $ ?e [?attr ...]
                    :where [?e ?attr ?value]]
               db e1 [:name :age])
            #{[:name "Ivan"] [:age 15]})))

    (testing "Empty coll handling"
      (is (= (d/q '[:find ?id
                    :in $ [?id ...]
                    :where [?id :age _]]
               [[1 :name "Ivan"]
                [2 :name "Petr"]]
               [])
            #{}))
      (is (= (d/q '[:find ?id
                    :in $ [[?id]]
                    :where [?id :age _]]
               [[1 :name "Ivan"]
                [2 :name "Petr"]]
               [])
            #{})))

    (testing "Placeholders"
      (is (= (d/q '[:find ?x ?z
                    :in [?x _ ?z]]
               [:x :y :z])
            #{[:x :z]}))
      (is (= (d/q '[:find ?x ?z
                    :in [[?x _ ?z]]]
               [[:x :y :z] [:a :b :c]])
            #{[:x :z] [:a :c]})))

    (testing "Error reporting"
      (is (thrown-with-msg? ExceptionInfo #"Cannot bind value :a to tuple \[\?a \?b\]"
            (d/q '[:find ?a ?b :in [?a ?b]] :a)))
      (is (thrown-with-msg? ExceptionInfo #"Cannot bind value :a to collection \[\?a \.\.\.\]"
            (d/q '[:find ?a :in [?a ...]] :a)))
      (is (thrown-with-msg? ExceptionInfo #"Not enough elements in a collection \[:a\] to bind tuple \[\?a \?b\]"
            (d/q '[:find ?a ?b :in [?a ?b]] [:a]))))))

(deftest test-nested-bindings
  (is (= (d/q '[:find  ?k ?v
                :in    [[?k ?v] ...]
                :where [(> ?v 1)]]
           {:a 1, :b 2, :c 3})
        #{[:b 2] [:c 3]}))

  (is (= (d/q '[:find  ?k ?min ?max
                :in    [[?k ?v] ...] ?minmax
                :where [(?minmax ?v) [?min ?max]]
                [(> ?max ?min)]]
           {:a [1 2 3 4]
            :b [5 6 7]
            :c [3]}
           #(vector (reduce min %) (reduce max %)))
        #{[:a 1 4] [:b 5 7]}))

  (is (= (d/q '[:find  ?k ?x
                :in    [[?k [?min ?max]] ...] ?range
                :where [(?range ?min ?max) [?x ...]]
                [(even? ?x)]]
           {:a [1 7]
            :b [2 4]}
           range)
        #{[:a 2] [:a 4] [:a 6]
          [:b 2]})))

(deftest test-built-in-regex
  (is (= (d/q '[:find  ?name
                :in    [?name ...] ?key
                :where [(re-pattern ?key) ?pattern]
                [(re-find ?pattern ?name)]]
           #{"abc" "abcX" "aXb"}
           "X")
        #{["abcX"] ["aXb"]})))

(deftest test-built-in-get
  (is (= (d/q '[:find ?m ?m-value
                :in [[?k ?m] ...] ?m-key
                :where [(get ?m ?m-key) ?m-value]]
           {:a {:b 1}
            :c {:d 2}}
           :d)
        #{[{:d 2} 2]})))

(deftest ^{:doc "issue-385"} test-join-unrelated
  (is (= #{}
        (d/q '[:find ?name
               :in $ ?my-fn
               :where [?e :person/name ?name]
               [(?my-fn) ?result]
               [(< ?result 3)]]
          (d/db-with (d/empty-db) [{:person/name "Joe"}])
          (fn [] 5)))))

(deftest ^{:doc "issue-425"} test-symbol-comparison
  (is (= [2]
        (d/q
          '[:find [?e ...]
            :where [?e :s b]]
          '[[1 :s a]
            [2 :s b]])))
  (let [tx (d/with (d/empty-db)
             '[{:db/id "e1", :s a}
               {:db/id "e2", :s b}])
        db (:db-after tx)
        e2 (get (:tempids tx) "e2")]
    (is (= [e2]
          (d/q
            '[:find [?e ...]
              :where [?e :s b]]
            db)))))

(deftest ^{:doc "issue-462"} test-constant-substitution
  (let [cnt+q (fn [query db & sources]
                (let [*cnt (volatile! 0)
                      db'  (d/filter db
                             (fn [db datom]
                               (vswap! *cnt inc)
                               true))
                      res  (apply d/q query db' sources)]
                  [@*cnt res]))
        schema {:a {:db/index true}
                :b {:db/index true}
                :c {:db/index true}}
        ;; Create 10 entities with string tempids
        tx     (d/with (d/empty-db schema)
                 (for [n (range 1 11)
                       attr [:a :b :c]]
                   [:db/add (str "e" n) attr (str n (name attr))]))
        db     (:db-after tx)
        e5     (get (:tempids tx) "e5")]
    (is (= [1 #{["5b"]}] (cnt+q '[:find ?v :in $ ?eid :where [?eid :b ?v]] db e5)))
    (is (= [1 #{[:b]}]   (cnt+q '[:find ?a :in $ ?eid :where [?eid ?a "5b"]] db e5)))
    (is (= [1 #{[e5]}]   (cnt+q '[:find ?e :where [?e :b "5b"]] db)))
    (is (= [1 #{[e5 :b "5b"]}] (cnt+q '[:find ?e ?a ?v :in $ ?e ?a :where [?e ?a ?v]] db e5 :b)))
    (is (= [2 #{[e5 :b "5b"]}] (cnt+q '[:find ?e2 ?a ?v :in $ ?a ?v :where [?e ?a ?v] [?e2 ?a ?v]] db :b "5b")))
    (is (= [3 #{[:a "5a"] [:b "5b"] [:c "5c"]}] (cnt+q '[:find ?a ?v :in $ ?e :where [?e ?a ?v]] db e5)))
    (is (= [1 #{[e5 :b]}] (cnt+q '[:find ?e ?a :where [?e ?a "5b"]] db)))
    (is (= [1 #{[e5 :b]}] (cnt+q '[:find ?e ?a :in $ ?v :where [?e ?a ?v]] db "5b")))
    (is (= [1 #{[e5 :b]}] (cnt+q '[:find ?e ?a :in $ [?v ...] :where [?e ?a ?v]] db ["5b"])))
    (is (= [1 #{[e5 :b]}] (cnt+q '[:find ?e ?a :where [(ground "5b") ?v] [?e ?a ?v]] db)))))
