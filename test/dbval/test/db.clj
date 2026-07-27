(ns dbval.test.db
  (:require
    [clojure.data]
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core]))

;; regression for the removal of the content-based `hash-db`: db values are
;; opaque handles (like Datomic databases) that hash and compare by reference
;; identity — content-based value semantics would have to realize a
;; potentially larger-than-memory database. Snapshots of the same store are
;; compared via `basis-tx`.
(deftest test-db-value-identity
  (let [conn (d/create-conn)
        db1  @conn
        db2  (d/db-with db1 [{:name "Ivan"}])]
    (testing "db values are not maps"
      (is (not (map? db1)))
      (is (nil? (:max-tx db1))))

    (testing "reference identity"
      (is (= db1 db1))
      (is (not= db1 db2))
      ;; equal content is not enough: both databases are empty,
      ;; but they are distinct handles (to distinct stores)
      (is (not= @(d/create-conn) @(d/create-conn))))

    (testing "snapshots are compared via basis-tx"
      (is (= (d/basis-tx db1) (d/basis-tx db1)))
      (is (pos? (compare (d/basis-tx db2) (d/basis-tx db1)))))))


(deftest test-bigdec-values
  ;; regression: BigDecimal used to fall through to the FoundationDB tuple
  ;; layer's Number fallback, which truncates via Number.longValue() -
  ;; a :price-adjustment/discount of 0.5M was stored as 0
  (let [conn (d/conn-from-db (d/empty-db {:amount {:db/index true}}))]
    (d/transact! conn [{:db/id "e1" :amount 0.5M}
                       {:db/id "e2" :amount 19.99M}
                       {:db/id "e3" :amount -3M}
                       {:db/id "e4" :amount 100M}])
    (let [db @conn
          e1 (:e (first (d/datoms db :avet :amount 0.5M)))]
      (testing "values survive transact/read round trips"
        (is (= 0.5M (:amount (d/entity db e1))))
        (is (= #{[0.5M] [19.99M] [-3M] [100M]}
              (d/q '[:find ?v :where [_ :amount ?v]] db))))

      (testing "avet lookup by value"
        (is (= [0.5M] (mapv :v (d/datoms db :avet :amount 0.5M)))))

      (testing "index-range in numeric order"
        (is (= [-3M 0.5M 19.99M 100M]
              (mapv :v (d/index-range db :amount -1000M 1000M)))))

      (testing "numerically equal decimals encode identically"
        (d/transact! conn [[:db/retract e1 :amount 0.50M]])
        (is (empty? (d/datoms @conn :avet :amount 0.5M)))))))

(deftest test-unsupported-number-type-throws
  ;; unknown Number subclasses are rejected instead of silently truncated
  (is (thrown? clojure.lang.ExceptionInfo
        (d/with (d/empty-db) [{:db/id "e1" :ratio 1/3}]))))

(deftest test-empty-vector-value
  ;; regression: storing an empty vector NPEd in `tuple` — the & rest args
  ;; are nil for zero components, but Tuple.addAll requires a List
  (let [tx (d/with (d/empty-db) [{:db/id "e1" :path []}])
        e1 (get (:tempids tx) "e1")
        db (:db-after tx)]
    (is (= [[]] (mapv :v (d/datoms db :eavt e1 :path))))))

(deftest test-diff
  ;; clojure.data/diff is deliberately unsupported: dbval databases may not
  ;; fit into memory, and a diff would have to realize both sides entirely.
  ;; The DB record still extends clojure.data/Diff, but only to throw —
  ;; otherwise diff would fall back to clojure.data's default map
  ;; implementation and diff the record's fields.
  (let [tx1 (d/with (d/empty-db) [{:db/id "e1" :a 1 :b 2}])
        db1 (:db-after tx1)
        e1 (get (:tempids tx1) "e1")
        db2 (d/db-with db1 [[:db/retract e1 :b 2]
                            [:db/add e1 :c 3]])]
    (is (thrown-msg? "clojure.data/diff is not supported on dbval databases, since it would realize both databases entirely in memory"
          (clojure.data/diff db1 db2)))))
