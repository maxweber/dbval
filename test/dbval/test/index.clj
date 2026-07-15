(ns dbval.test.index
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(deftest test-datoms
  (let [dvec #(vector (:e %) (:a %) (:v %))
        tx (d/with (d/empty-db {:age {:db/index true}})
             [{:db/id "e1" :name "Petr" :age 44}
              {:db/id "e2" :name "Ivan" :age 25}
              {:db/id "e3" :name "Sergey" :age 11}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]
    (testing "Main indexes, sort order"
      ;; With UUIDs, order in indexes depends on UUID comparison
      ;; Just verify all expected datoms are present
      (is (= #{[e1 :age 44] [e2 :age 25] [e3 :age 11]
               [e1 :name "Petr"] [e2 :name "Ivan"] [e3 :name "Sergey"]}
            (set (map dvec (d/datoms db :aevt)))))

      (is (= #{[e1 :age 44] [e1 :name "Petr"]
               [e2 :age 25] [e2 :name "Ivan"]
               [e3 :age 11] [e3 :name "Sergey"]}
            (set (map dvec (d/datoms db :eavt)))))

      ;; :avet is sorted by attribute, then value
      (is (= [[e3 :age 11] [e2 :age 25] [e1 :age 44]]
            (map dvec (d/datoms db :avet))))) ;; name non-indexed, excluded from avet

    (testing "Components filtration"
      (is (= #{[e1 :age 44] [e1 :name "Petr"]}
            (set (map dvec (d/datoms db :eavt e1)))))

      (is (= [[e1 :age 44]]
            (map dvec (d/datoms db :eavt e1 :age))))

      (is (= [[e3 :age 11] [e2 :age 25] [e1 :age 44]]
            (map dvec (d/datoms db :avet :age)))))

    (testing "Error reporting"
      (d/datoms db :avet) ;; no error

      (is (thrown-msg? "Attribute :name should be marked as :db/index true"
            (d/datoms db :avet :name)))

      (is (thrown-msg? "Attribute :alias should be marked as :db/index true"
            (d/datoms db :avet :alias)))

      (is (thrown-msg? "Attribute :name should be marked as :db/index true"
            (d/datoms db :avet :name "Ivan")))

      (is (thrown-msg? "Attribute :name should be marked as :db/index true"
            (d/datoms db :avet :name "Ivan" e1))))

    (testing "Sequence compare issue-470"
      (let [db* (fn []
                  (let [tx (d/with (d/empty-db {:path {:db/index true}})
                             [{:db/id "e1" :path [1 2]}
                              {:db/id "e2" :path [1 2 3]}])]
                    {:db (:db-after tx)
                     :e1 (get (:tempids tx) "e1")
                     :e2 (get (:tempids tx) "e2")}))]
        (let [{:keys [db e1 e2]} (db*)]
          (is (= [] (mapv :e (d/datoms db :avet :path [1]))))
          (is (= [] (mapv :e (d/datoms db :avet :path [1 1]))))
          (is (= [e1] (mapv :e (d/datoms db :avet :path [1 2]))))
          (is (= [e1] (mapv :e (d/datoms db :avet :path (list 1 2)))))
          (is (= [e1] (mapv :e (d/datoms db :avet :path (butlast [1 2 3])))))
          (is (= [] (mapv :e (d/datoms db :avet :path [1 3]))))
          (is (= [] (mapv :e (d/datoms db :avet :path [1 2 2]))))
          (is (= [e2] (mapv :e (d/datoms db :avet :path [1 2 3]))))
          (is (= [e2] (mapv :e (d/datoms db :avet :path (list 1 2 3)))))
          (is (= [e2] (mapv :e (d/datoms db :avet :path (butlast [1 2 3 4])))))
          (is (= [] (mapv :e (d/datoms db :avet :path [1 2 4]))))
          (is (= [] (mapv :e (d/datoms db :avet :path [1 2 3 4])))))))))

(deftest test-datom
  (let [dvec #(when % (vector (:e %) (:a %) (:v %)))
        tx (d/with (d/empty-db {:age {:db/index true}})
             [{:db/id "e1" :name "Petr" :age 44}
              {:db/id "e2" :name "Ivan" :age 25}
              {:db/id "e3" :name "Sergey" :age 11}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]
    ;; First datom in eavt order - depends on UUID ordering, just check it's one of the entities
    (let [first-datom (d/find-datom db :eavt)]
      (is (some #{(:e first-datom)} [e1 e2 e3])))

    ;; With specific entity ID
    (is (= [e1 :age 44] (dvec (d/find-datom db :eavt e1))))
    (is (= [e1 :age 44] (dvec (d/find-datom db :eavt e1 :age))))
    (is (= [e1 :name "Petr"] (dvec (d/find-datom db :eavt e1 :name))))
    (is (= [e1 :name "Petr"] (dvec (d/find-datom db :eavt e1 :name "Petr"))))

    (is (= [e2 :age 25] (dvec (d/find-datom db :eavt e2))))
    (is (= [e2 :age 25] (dvec (d/find-datom db :eavt e2 :age))))
    (is (= [e2 :name "Ivan"] (dvec (d/find-datom db :eavt e2 :name))))

    (is (= nil (dvec (d/find-datom db :eavt e1 :name "Ivan"))))
    (is (= nil (dvec (d/find-datom db :eavt (random-uuid)))))

    ;; issue-477
    (is (= nil (d/find-datom (d/empty-db) :eavt)))
    (is (= nil (d/find-datom (d/empty-db {:age {:db/index true}}) :eavt)))))

(deftest test-seek-datoms
  (let [dvec #(vector (:e %) (:a %) (:v %))
        tx (d/with (d/empty-db {:name {:db/index true}
                                 :age  {:db/index true}})
             [{:db/id "e1" :name "Petr" :age 44}
              {:db/id "e2" :name "Ivan" :age 25}
              {:db/id "e3" :name "Sergey" :age 11}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]

    (testing "Non-termination"
      ;; Seek from age 10 should return all ages >= 10, then all names
      (let [result (d/seek-datoms db :avet :age 10)]
        (is (= #{[e3 :age 11] [e2 :age 25] [e1 :age 44]
                 [e2 :name "Ivan"] [e1 :name "Petr"] [e3 :name "Sergey"]}
              (set (map dvec result))))))

    (testing "Closest value lookup"
      (is (= #{[e1 :name "Petr"] [e3 :name "Sergey"]}
            (set (map dvec (d/seek-datoms db :avet :name "P"))))))

    (testing "Exact value lookup"
      (is (= #{[e1 :name "Petr"] [e3 :name "Sergey"]}
            (set (map dvec (d/seek-datoms db :avet :name "Petr"))))))

    (is (thrown-msg? "Attribute :alias should be marked as :db/index true"
          (d/seek-datoms db :avet :alias)))))

(deftest test-rseek-datoms
  (let [dvec #(vector (:e %) (:a %) (:v %))
        tx (d/with (d/empty-db {:name {:db/index true}
                                 :age  {:db/index true}})
             [{:db/id "e1" :name "Petr" :age 44}
              {:db/id "e2" :name "Ivan" :age 25}
              {:db/id "e3" :name "Sergey" :age 11}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]

    (testing "Non-termination"
      (let [result (d/rseek-datoms db :avet :name "Petr")]
        (is (= #{[e1 :name "Petr"] [e2 :name "Ivan"]
                 [e1 :age 44] [e2 :age 25] [e3 :age 11]}
              (set (map dvec result))))))

    (testing "Closest value lookup"
      (is (= #{[e2 :age 25] [e3 :age 11]}
            (set (map dvec (d/rseek-datoms db :avet :age 26))))))

    (testing "Exact value lookup"
      (is (= #{[e2 :age 25] [e3 :age 11]}
            (set (map dvec (d/rseek-datoms db :avet :age 25))))))

    (is (thrown-msg? "Attribute :alias should be marked as :db/index true"
          (d/rseek-datoms db :avet :alias)))))

(deftest test-rseek-datoms-with-history
  ;; regression: -rseek-datoms fed a descending stream into datoms-filter,
  ;; which assumes ascending order — retract tombstones leaked out, retracted
  ;; datoms were returned and live datoms were dropped
  (let [dvec #(vector (:a %) (:v %) (:added %))]
    (testing "retracted datom is invisible, live datoms are kept"
      (let [tx (d/with (d/empty-db)
                 [{:db/id "e1" :name "Ivan" :age 30}])
            e1 (get (:tempids tx) "e1")
            db (d/db-with (:db-after tx) [[:db/retract e1 :age 30]])]
        (is (= [[:name "Ivan" true]]
              (map dvec (d/rseek-datoms db :eavt e1))))))

    (testing "retracted and re-added datom is visible exactly once"
      (let [tx (d/with (d/empty-db)
                 [{:db/id "e1" :name "Ivan" :age 30}])
            e1 (get (:tempids tx) "e1")
            db (-> (:db-after tx)
                 (d/db-with [[:db/retract e1 :age 30]])
                 (d/db-with [[:db/add e1 :age 31]]))]
        (is (= [[:name "Ivan" true] [:age 31 true]]
              (map dvec (d/rseek-datoms db :eavt e1))))))

    (testing "datom added and retracted in the same transaction is invisible"
      (let [tx (d/with (d/empty-db)
                 [{:db/id "e1" :name "Ivan"}])
            e1 (get (:tempids tx) "e1")
            db (d/db-with (:db-after tx)
                 [[:db/add e1 :age 30]
                  [:db/retract e1 :age 30]])]
        (is (= [[:name "Ivan" true]]
              (map dvec (d/rseek-datoms db :eavt e1))))))

    (testing "rseek-datoms is the exact reverse of datoms"
      (let [dvec+e #(vector (:e %) (:a %) (:v %) (:added %))
            tx (d/with (d/empty-db {:age {:db/index true}})
                 [{:db/id "e1" :name "Ivan" :age 30}
                  {:db/id "e2" :name "Oleg" :age 40}])
            e1 (get (:tempids tx) "e1")
            db (-> (:db-after tx)
                 (d/db-with [[:db/retract e1 :age 30]])
                 (d/db-with [[:db/add e1 :age 31]]))]
        (doseq [index [:eavt :aevt :avet]]
          (is (= (reverse (map dvec+e (d/datoms db index)))
                (map dvec+e (d/rseek-datoms db index)))
            (str "index " index)))))))

(deftest test-index-range
  (let [dvec #(vector (:e %) (:a %) (:v %))
        tx (d/with (d/empty-db {:name {:db/index true}
                                 :age  {:db/index true}})
             [{:db/id "e1" :name "Ivan"   :age 15}
              {:db/id "e2" :name "Oleg"   :age 20}
              {:db/id "e3" :name "Sergey" :age 7}
              {:db/id "e4" :name "Pavel"  :age 45}
              {:db/id "e5" :name "Petr"   :age 20}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")
        e4 (get (:tempids tx) "e4")
        e5 (get (:tempids tx) "e5")]
    (is (= [[e5 :name "Petr"]]
          (map dvec (d/index-range db :name "Pe" "S"))))
    (is (= #{[e2 :name "Oleg"] [e4 :name "Pavel"] [e5 :name "Petr"] [e3 :name "Sergey"]}
          (set (map dvec (d/index-range db :name "O" "Sergey")))))

    (is (= #{[e1 :name "Ivan"] [e2 :name "Oleg"]}
          (set (map dvec (d/index-range db :name nil "P")))))
    (is (= [[e3 :name "Sergey"]]
          (map dvec (d/index-range db :name "R" nil))))
    (is (= #{[e1 :name "Ivan"] [e2 :name "Oleg"] [e4 :name "Pavel"]
             [e5 :name "Petr"] [e3 :name "Sergey"]}
          (set (map dvec (d/index-range db :name nil nil)))))

    (is (= #{[e1 :age 15] [e2 :age 20] [e5 :age 20]}
          (set (map dvec (d/index-range db :age 15 20)))))
    (is (= #{[e3 :age 7] [e1 :age 15] [e2 :age 20] [e5 :age 20] [e4 :age 45]}
          (set (map dvec (d/index-range db :age 7 45)))))
    (is (= #{[e3 :age 7] [e1 :age 15] [e2 :age 20] [e5 :age 20] [e4 :age 45]}
          (set (map dvec (d/index-range db :age 0 100)))))

    (is (thrown-msg? "Attribute :alias should be marked as :db/index true"
          (d/index-range db :alias "e" "u")))))

(deftest test-index-range-snapshot-isolation
  ;; regression: -index-range used to omit the :max-tx filter, so a stale
  ;; db snapshot could see datoms transacted after it was taken
  (testing "datoms added after the snapshot are invisible"
    (let [db1 (:db-after (d/with (d/empty-db {:age {:db/index true}})
                           [{:db/id "e1" :age 10}]))
          db2 (d/db-with db1 [{:db/id "e2" :age 20}])]
      (is (= [10] (mapv :v (d/index-range db1 :age 0 100))))
      (is (= [10 20] (mapv :v (d/index-range db2 :age 0 100))))))

  (testing "datoms retracted after the snapshot are still visible"
    (let [tx  (d/with (d/empty-db {:age {:db/index true}})
                [{:db/id "e1" :age 10}])
          db1 (:db-after tx)
          e1  (get (:tempids tx) "e1")
          db2 (d/db-with db1 [[:db/retract e1 :age 10]])]
      (is (= [10] (mapv :v (d/index-range db1 :age 0 100))))
      (is (= [] (mapv :v (d/index-range db2 :age 0 100)))))))
