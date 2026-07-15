(ns dbval.test.components
  (:require
    [clojure.edn :as edn]
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(t/use-fixtures :once tdc/no-namespace-maps)


(deftest test-components
  (is (thrown-msg? "Bad attribute specification for :profile: {:db/isComponent true} should also have {:db/valueType :db.type/ref}"
        (d/empty-db {:profile {:db/isComponent true}})))
  (is (thrown-msg? "Bad attribute specification for {:profile {:db/isComponent \"aaa\"}}, expected one of #{true false}"
        (d/empty-db {:profile {:db/isComponent "aaa" :db/valueType :db.type/ref}})))

  (let [db* (fn []
              (let [tx (d/with
                         (d/empty-db {:profile {:db/valueType   :db.type/ref
                                                :db/isComponent true}})
                         [{:db/id "e1" :name "Ivan" :profile "e3"}
                          {:db/id "e3" :email "@3"}
                          {:db/id "e4" :email "@4"}])]
                {:db (:db-after tx)
                 :e1 (get (:tempids tx) "e1")
                 :e3 (get (:tempids tx) "e3")
                 :e4 (get (:tempids tx) "e4")}))
        visible #(edn/read-string (pr-str %))
        touched #(visible (d/touch %))]

    (testing "touch"
      (let [{:keys [db e1 e3]} (db*)]
        (is (= (touched (d/entity db e1))
               {:db/id e1
                :name "Ivan"
                :profile {:db/id e3
                          :email "@3"}})))
      (let [{:keys [db e1 e3 e4]} (db*)]
        (is (= (touched (d/entity (d/db-with db [[:db/add e3 :profile e4]]) e1))
               {:db/id e1
                :name "Ivan"
                :profile {:db/id e3
                          :email "@3"
                          :profile {:db/id e4
                                    :email "@4"}}}))))
    (testing "retractEntity"
      (let [{:keys [db e1 e3]} (db*)
            db' (d/db-with db [[:db.fn/retractEntity e1]])]
        (is (= (d/q '[:find ?a ?v :in $ ?e :where [?e ?a ?v]] db' e1)
               #{}))
        (is (= (d/q '[:find ?a ?v :in $ ?e :where [?e ?a ?v]] db' e3)
               #{}))))

    (testing "retractAttribute"
      (let [{:keys [db e1 e3]} (db*)
            db' (d/db-with db [[:db.fn/retractAttribute e1 :profile]])]
        (is (= (d/q '[:find ?a ?v :in $ ?e :where [?e ?a ?v]] db' e3)
               #{}))))

    (testing "reverse navigation"
      (let [{:keys [db e1 e3]} (db*)]
        (is (= (visible (:_profile (d/entity db e3)))
               {:db/id e1}))))))

(deftest test-components-multival
  (let [db* (fn []
              (let [tx (d/with
                         (d/empty-db {:profile {:db/valueType   :db.type/ref
                                                :db/cardinality :db.cardinality/many
                                                :db/isComponent true}})
                         [{:db/id "e1" :name "Ivan" :profile ["e3" "e4"]}
                          {:db/id "e3" :email "@3"}
                          {:db/id "e4" :email "@4"}])]
                {:db (:db-after tx)
                 :e1 (get (:tempids tx) "e1")
                 :e3 (get (:tempids tx) "e3")
                 :e4 (get (:tempids tx) "e4")}))
        visible #(edn/read-string (pr-str %))
        touched #(visible (d/touch %))]

    (testing "touch"
      (let [{:keys [db e1 e3 e4]} (db*)]
        (is (= (touched (d/entity db e1))
               {:db/id e1
                :name "Ivan"
                :profile #{{:db/id e3 :email "@3"}
                           {:db/id e4 :email "@4"}}}))))

    (testing "retractEntity"
      (let [{:keys [db e1 e3 e4]} (db*)
            db' (d/db-with db [[:db.fn/retractEntity e1]])]
        (is (= (d/q '[:find ?a ?v :in $ [?e ...] :where [?e ?a ?v]] db' [e1 e3 e4])
               #{}))))

    (testing "retractAttribute"
      (let [{:keys [db e1 e3 e4]} (db*)
            db' (d/db-with db [[:db.fn/retractAttribute e1 :profile]])]
        (is (= (d/q '[:find ?a ?v :in $ [?e ...] :where [?e ?a ?v]] db' [e3 e4])
               #{}))))

    (testing "reverse navigation"
      (let [{:keys [db e1 e3]} (db*)]
        (is (= (visible (:_profile (d/entity db e3)))
               {:db/id e1}))))))
