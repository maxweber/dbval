(ns dbval.test.query-fns
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc])
  (:import
       [clojure.lang ExceptionInfo]))

(deftest test-query-fns
  (testing "predicate without free variables"
    (is (= (d/q '[:find ?x
                  :in [?x ...]
                  :where [(> 2 1)]] [:a :b :c])
          #{[:a] [:b] [:c]})))

  (let [tx (d/with (d/empty-db {:parent {:db/valueType :db.type/ref}})
             [{:db/id "e1", :name  "Ivan",  :age   15}
              {:db/id "e2", :name  "Petr",  :age   22, :height 240, :parent "e1"}
              {:db/id "e3", :name  "Slava", :age   37, :parent "e2"}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]

    (testing "ground"
      (is (= (d/q '[:find ?vowel
                    :where [(ground [:a :e :i :o :u]) [?vowel ...]]])
            #{[:a] [:e] [:i] [:o] [:u]})))

    (testing "get-else"
      (is (= (d/q '[:find ?e ?age ?height
                    :where [?e :age ?age]
                    [(get-else $ ?e :height 300) ?height]] db)
            #{[e1 15 300] [e2 22 240] [e3 37 300]}))

      (is (thrown-with-msg? ExceptionInfo #"get-else: nil default value is not supported"
            (d/q '[:find ?e ?height
                   :where [?e :age]
                   [(get-else $ ?e :height nil) ?height]] db))))

    (testing "get-some"
      (is (= (d/q '[:find ?e ?a ?v
                    :where [?e :name _]
                    [(get-some $ ?e :height :age) [?a ?v]]] db)
            #{[e1 :age 15]
              [e2 :height 240]
              [e3 :age 37]})))

    (testing "missing?"
      (is (= (d/q '[:find ?e ?age
                    :in $
                    :where [?e :age ?age]
                    [(missing? $ ?e :height)]] db)
            #{[e1 15] [e3 37]})))

    (testing "missing? back-ref"
      (is (= (d/q '[:find ?e
                    :in $
                    :where [?e :age ?age]
                    [(missing? $ ?e :_parent)]] db)
            #{[e3]})))

    (testing "Built-ins"
      (is (= (d/q '[:find  ?e1 ?e2
                    :where [?e1 :age ?a1]
                    [?e2 :age ?a2]
                    [(< ?a1 18 ?a2)]] db)
            #{[e1 e2] [e1 e3]}))
      (is (= (d/q '[:find  ?a1
                    :where [_ :age ?a1]
                    [(< ?a1 22)]] db)
            #{[15]}))
      (is (= (d/q '[:find  ?a1
                    :where [_ :age ?a1]
                    [(<= ?a1 22)]] db)
            #{[15] [22]}))
      (is (= (d/q '[:find  ?a1
                    :where [_ :age ?a1]
                    [(> ?a1 22)]] db)
            #{[37]}))
      (is (= (d/q '[:find  ?a1
                    :where [_ :age ?a1]
                    [(>= ?a1 22)]] db)
            #{[22] [37]}))
      (testing "compare values of different types"
        (is (= (d/q '[:find  ?e
                      :where [?e]
                      [(< ?e 1)]] [[0] [1] [""]])
              #{[0]}))
        (is (= (d/q '[:find  ?e
                      :where [?e]
                      [(<= ?e 1)]] [[0] [1] [""]])
              #{[0] [1]}))
        (is (= (d/q '[:find  ?e
                      :where [?e]
                      [(> ?e 1)]] [[0] [1] [""]])
              #{[""]}))
        (is (= (d/q '[:find  ?e
                      :where [?e]
                      [(>= ?e 1)]] [[0] [1] [""]])
              #{[1] [""]})))

      (is (= (d/q '[:find  ?x ?c
                    :in    [?x ...]
                    :where [(count ?x) ?c]]
               ["a" "abc"])
            #{["a" 1] ["abc" 3]})))

    (testing "Built-in vector, hashmap"
      (is (= (d/q '[:find [?tx-data ...]
                    :where
                    [(ground :db/add) ?op]
                    [(vector ?op -1 :attr 12) ?tx-data]])
            [[:db/add -1 :attr 12]]))

      (is (= (d/q '[:find [?tx-data ...]
                    :where
                    [(hash-map :db/id -1 :age 92 :name "Aaron") ?tx-data]])
            [{:db/id -1 :age 92 :name "Aaron"}])))

    (testing "Passing predicate as source"
      (is (= (d/q '[:find  ?e
                    :in    $ ?adult
                    :where [?e :age ?a]
                    [(?adult ?a)]]
               db
               #(> % 18))
            #{[e2] [e3]})))

    (testing "Calling a function"
      (is (= (d/q '[:find  ?e1 ?e2 ?e3
                    :where [?e1 :age ?a1]
                    [?e2 :age ?a2]
                    [?e3 :age ?a3]
                    [(+ ?a1 ?a2) ?a12]
                    [(= ?a12 ?a3)]]
               db)
            #{[e1 e2 e3] [e2 e1 e3]})))

    (testing "Two conflicting function values for one binding."
      (is (= (d/q '[:find  ?n
                    :where [(identity 1) ?n]
                    [(identity 2) ?n]])
            #{})))

    (testing "Destructured conflicting function values for two bindings."
      (is (= (d/q '[:find  ?n ?x
                    :where [(identity [3 4]) [?n ?x]]
                    [(identity [1 2]) [?n ?x]]])
            #{})))

    (testing "Rule bindings interacting with function binding. (fn, rule)"
      (is (= (d/q '[:find  ?n
                    :in $ %
                    :where [(identity 2) ?n]
                    (my-vals ?n)]
               db
               '[[(my-vals ?x)
                  [(identity 1) ?x]]
                 [(my-vals ?x)
                  [(identity 2) ?x]]
                 [(my-vals ?x)
                  [(identity 3) ?x]]])
            #{[2]})))

    (testing "Rule bindings interacting with function binding. (rule, fn)"
      (is (= (d/q '[:find  ?n
                    :in $ %
                    :where (my-vals ?n)
                    [(identity 2) ?n]]
               db
               '[[(my-vals ?x)
                  [(identity 1) ?x]]
                 [(my-vals ?x)
                  [(identity 2) ?x]]
                 [(my-vals ?x)
                  [(identity 3) ?x]]])
            #{[2]})))

    (testing "Conflicting relational bindings with function binding. (rel, fn)"
      (is (= (d/q '[:find  ?age
                    :where [_ :age ?age]
                    [(identity 100) ?age]]
               db)
            #{})))

    (testing "Conflicting relational bindings with function binding. (fn, rel)"
      (is (= (d/q '[:find  ?age
                    :where [(identity 100) ?age]
                    [_ :age ?age]]
               db)
            #{})))

    (testing "Function on empty rel"
      (is (= (d/q '[:find  ?e ?y
                    :where [?e :salary ?x]
                    [(+ ?x 100) ?y]]
               [[0 :age 15] [1 :age 35]])
            #{})))

    (testing "Returning nil from function filters out tuple from result"
      (is (= (d/q '[:find ?x
                    :in    [?in ...] ?f
                    :where [(?f ?in) ?x]]
               [1 2 3 4]
               #(when (even? %) %))
            #{[2] [4]})))

    (testing "Result bindings"
      (is (= (d/q '[:find ?a ?c
                    :in ?in
                    :where [(ground ?in) [?a _ ?c]]]
               [:a :b :c])
            #{[:a :c]}))

      (is (= (d/q '[:find ?in
                    :in ?in
                    :where [(ground ?in) _]]
               :a)
            #{[:a]}))

      (is (= (d/q '[:find ?x ?z
                    :in ?in
                    :where [(ground ?in) [[?x _ ?z]...]]]
               [[:a :b :c] [:d :e :f]])
            #{[:a :c] [:d :f]}))

      (is (= (d/q '[:find ?in
                    :in [?in ...]
                    :where [(ground ?in) _]]
               [])
            #{})))))

(deftest test-predicates
  (let [tx (d/with (d/empty-db)
             [{:db/id "e1" :name "Ivan" :age 10}
              {:db/id "e2" :name "Ivan" :age 20}
              {:db/id "e3" :name "Oleg" :age 10}
              {:db/id "e4" :name "Oleg" :age 20}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")
        e4 (get (:tempids tx) "e4")]
    ;; plain predicate
    (is (= (d/q '[:find  ?e ?a
                  :where [?e :age ?a]
                  [(> ?a 10)]] db)
          #{[e2 20] [e4 20]}))

    ;; join in predicate - compare by UUIDs
    ;; UUIDs are compared lexicographically, so we can't predict which is less
    ;; Just verify result count and structure
    (let [result (d/q '[:find  ?e ?e2
                        :where [?e  :name]
                        [?e2 :name]
                        [(not= ?e ?e2)]] db)]
      (is (= 12 (count result))))  ;; 4 entities * 3 other entities = 12 pairs

    ;; join with extra symbols - same issue with UUID comparison
    (let [result (d/q '[:find  ?e ?e2
                        :where [?e  :age ?a]
                        [?e2 :age ?a2]
                        [(not= ?e ?e2)]] db)]
      (is (= 12 (count result))))  ;; 4 * 3 = 12 pairs

    ;; empty result
    (is (= (d/q '[:find  ?e ?e2
                  :where [?e  :name "Ivan"]
                  [?e2 :name "Oleg"]
                  [(= ?e ?e2)]] db)
          #{}))

    ;; pred over const, true
    (is (= (d/q '[:find  ?e
                  :in $ ?eid
                  :where [?e :name "Ivan"]
                  [?e :age 20]
                  [(= ?e ?eid)]] db e2)
          #{[e2]}))

    ;; pred over const, false
    (is (= (d/q '[:find  ?e
                  :in $ ?eid
                  :where [?e :name "Ivan"]
                  [?e :age 20]
                  [(= ?e ?eid)]] db e1)
          #{}))

    (let [pred (fn [db e a]
                 (= a (:age (d/entity db e))))]
      (is (= (d/q '[:find ?e
                    :in $ ?pred
                    :where [?e :age ?a]
                    [(?pred $ ?e 10)]]
               db pred)
            #{[e1] [e3]})))))

(deftest test-exceptions
  (is (thrown-msg? "Unknown predicate 'fun in [(fun ?e)]"
        (d/q '[:find ?e
               :in   [?e ...]
               :where [(fun ?e)]]
          [1])))

  (is (thrown-msg? "Unknown function 'fun in [(fun ?e) ?x]"
        (d/q '[:find ?e ?x
               :in   [?e ...]
               :where [(fun ?e) ?x]]
          [1])))

  (is (thrown-msg? "Insufficient bindings: #{?x} not bound in [(zero? ?x)]"
        (d/q '[:find ?x
               :where [(zero? ?x)]])))

  (is (thrown-msg? "Insufficient bindings: #{?x} not bound in [(inc ?x) ?y]"
        (d/q '[:find ?x
               :where [(inc ?x) ?y]])))

  (is (thrown-msg? "Where uses unknown source vars: [$2]"
        (d/q '[:find ?x
               :where [?x] [(zero? $2 ?x)]])))

  (is (thrown-msg? "Where uses unknown source vars: [$]"
        (d/q '[:find  ?x
               :in    $2
               :where [$2 ?x] [(zero? $ ?x)]]))))

(deftest test-issue-180
  (is (= #{}
        (d/q '[:find ?e ?a
               :where [_ :pred ?pred]
               [?e :age ?a]
               [(?pred ?a)]]
          (d/db-with (d/empty-db) [{:age 20}])))))

(defn sample-query-fn []
  42)

(deftest test-symbol-resolution
     (is (= 42 (d/q '[:find ?x .
                      :where [(dbval.test.query-fns/sample-query-fn) ?x]]))))

(deftest test-issue-445
  (let [tx (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
             [{:db/id "e1" :name "Ivan" :age 15}
              {:db/id "e2" :name "Petr" :age 22 :height 240}])
        db (:db-after tx)]
    (testing "get-else using lookup ref"
      (is (= "Unknown"
            (d/q '[:find ?height .
                   :in $ ?e
                   :where [(get-else $ ?e :height "Unknown") ?height]]
              db
              [:name "Ivan"]))))

    (testing "get-some using lookup ref"
      (is (= #{[[:name "Petr"] :age 22]}
            (d/q '[:find ?e ?a ?v
                   :in $ ?e
                   :where [(get-some $ ?e :weight :age :height) [?a ?v]]]
              db
              [:name "Petr"]))))))
