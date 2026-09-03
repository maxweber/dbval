(ns dbval.test.validation
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.test.core :as tdc]))


(deftest test-with-validation
  (let [db* (fn []
              (d/empty-db {:profile {:db/valueType :db.type/ref}
                           :id {:db/unique :db.unique/identity}}))]
    (are [tx] (thrown-with-msg? Throwable #"Expected UUID or lookup ref for :db/id" (d/db-with (db*) tx))
      [{:db/id #"" :name "Ivan"}])

    (are [tx] (thrown-with-msg? Throwable #"Bad entity attribute" (d/db-with (db*) tx))
      [[:db/add "e1" nil "Ivan"]]
      [[:db/add "e1" 17 "Ivan"]]
      [{:db/id "e1" 17 "Ivan"}])

    (are [tx] (thrown-with-msg? Throwable #"Cannot store nil as a value" (d/db-with (db*) tx))
      [[:db/add "e1" :name nil]]
      [{:db/id "e1" :name nil}]
      [[:db/add "e1" :id nil]]
      [{:db/id "e1" :id "A"}
       {:db/id "e1" :id nil}])

    (are [tx] (thrown-with-msg? Throwable #"Expected UUID or lookup ref for entity id" (d/db-with (db*) tx))
      [[:db/add nil :name "Ivan"]]
      [[:db/add {} :name "Ivan"]]
      [[:db/add "e1" :profile #"regexp"]]
      [{:db/id "e1" :profile #"regexp"}])

    (is (thrown-with-msg? Throwable #"Unknown operation" (d/db-with (db*) [["aaa" :name "Ivan"]])))
    (is (thrown-with-msg? Throwable #"Bad entity type at" (d/db-with (db*) [:db/add "aaa" :name "Ivan"])))
    (is (thrown-with-msg? Throwable #"Bad transaction data" (d/db-with (db*) {:profile "aaa"})))))

(deftest test-unique
  (let [db* (fn []
              (:db-after (d/with (d/empty-db {:name {:db/unique :db.unique/value}})
                           [[:db/add "e1" :name "Ivan"]
                            [:db/add "e2" :name "Petr"]])))]
    ;; Unique constraint on "Ivan"
    (is (thrown-with-msg? Throwable #"unique constraint"
          (d/db-with (db*) [[:db/add "e3" :name "Ivan"]])))
    ;; Unique constraint on "Petr"
    (is (thrown-with-msg? Throwable #"unique constraint"
          (d/db-with (db*) [{:db/id "e3" :name "Petr"}])))
    ;; New name "Igor" should work
    (d/db-with (db*) [[:db/add "e3" :name "Igor"]])
    ;; Different attribute :nick should work
    (d/db-with (db*) [[:db/add "e3" :nick "Ivan"]])))

(deftest test-unsupported-value-types
  ;; rejected with attribute context at the serialization boundary instead
  ;; of an opaque "pack failed" from inside the byte encoder
  (let [db* (fn [] (d/empty-db))]
    (are [tx] (thrown-with-msg? Throwable #"unsupported type"
                (d/db-with (db*) tx))
      [[:db/add "e1" :amount 1/3]]
      [{:db/id "e1" :amount (java.util.concurrent.atomic.AtomicLong. 5)}]
      [[:db/add "e1" :initial \a]]
      ;; nested inside a stored collection
      [[:db/add "e1" :pair [1 1/3]]])
    (let [ex (try
               (d/db-with (db*) [[:db/add "e1" :amount 1/3]])
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (= :transact/unsupported-value-type (:error (ex-data ex))))
      (is (= :amount (:attribute (ex-data ex)))))
    ;; the read path rejects unsupported search-pattern values the same way
    (is (thrown-with-msg? Throwable #"unsupported type"
          (vec (d/datoms (d/empty-db {:amount {:db/index true}})
                         :avet :amount 1/3))))))

(deftest test-decimal-size-cap
  ;; a BigDecimal is the one number type whose encoded size grows with the
  ;; value (one byte per significant digit); it obeys the same inline cap
  ;; as strings instead of writing oversized index keys
  (let [huge (java.math.BigDecimal.
               (java.math.BigInteger. (.repeat "9" 60001)))]
    (is (thrown-with-msg? Throwable #"significant"
          (d/db-with (d/empty-db) [[:db/add "e1" :amount huge]]))))
  ;; scientific notation with a huge exponent stays tiny and must pass
  (let [db (d/db-with (d/empty-db) [[:db/add "e1" :amount 1E+100000M]])]
    (is (= 1E+100000M (d/q '[:find ?v . :where [_ :amount ?v]] db)))))

(deftest test-oversized-nested-and-total-keys
  ;; a string nested inside a tuple value obeys the same inline cap as a
  ;; top-level string (it used to bypass validate-inline-size entirely)
  (is (thrown-with-msg? Throwable #"more than"
        (d/db-with (d/empty-db)
                   [[:db/add "e1" :pair [1 (.repeat "x" 70000)]]])))
  ;; components can each pass the per-value cap and still sum past the
  ;; 64 KiB SlateDB key limit; the packed-key backstop rejects those
  (let [s (.repeat "y" 40000)]
    (is (thrown-with-msg? Throwable #"index-key limit"
          (d/db-with (d/empty-db) [[:db/add "e1" :pair [s s s]]])))))

(deftest test-unpaired-surrogate-strings
  ;; rejected with attribute context at the serialization boundary, not
  ;; from inside the byte encoder with the datom dumped into ex-data
  (let [ex (try
             (d/db-with (d/empty-db) [[:db/add "e1" :caption "a\ud800b"]])
             nil
             (catch clojure.lang.ExceptionInfo e e))]
    (is (= :transact/invalid-string (:error (ex-data ex))))
    (is (= :caption (:attribute (ex-data ex))))
    (is (nil? (:tuple (ex-data ex)))))
  ;; nested inside a tuple value
  (is (thrown-with-msg? Throwable #"unpaired surrogate"
        (d/db-with (d/empty-db) [[:db/add "e1" :pair ["a\ud800b" 1]]]))))
