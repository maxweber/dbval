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
