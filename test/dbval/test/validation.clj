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
