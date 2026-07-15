(ns dbval.test.filter
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(deftest test-filter-db
  (let [schema {:name {:db/unique :db.unique/identity}
                :aka {:db/cardinality :db.cardinality/many}}
        empty-db* (fn [] (d/empty-db schema))
        empty-db (empty-db*)
        make-db (fn []
                  (let [tx (d/with (empty-db*)
                             [{:db/id "petr"
                               :name  "Petr"
                               :email "petya@spb.ru"
                               :aka   ["I" "Great"]
                               :password "<SECRET>"}
                              {:db/id "ivan"
                               :name  "Ivan"
                               :aka   ["Terrible" "IV"]
                               :password "<PROTECTED>"}
                              {:db/id "nikolai"
                               :name  "Nikolai"
                               :aka   ["II"]
                               :password "<UNKWOWN>"}])]
                    {:db (:db-after tx)
                     :petr (get (:tempids tx) "petr")
                     :ivan (get (:tempids tx) "ivan")
                     :nikolai (get (:tempids tx) "nikolai")}))
        {:keys [db petr ivan nikolai]} (make-db)
        remove-pass (fn [_ datom] (not= :password (:a datom)))
        remove-ivan (fn [_ datom] (not= ivan (:e datom)))
        long-akas   (fn [udb datom] (or (not= :aka (:a datom))
                                      ;; has just 1 aka
                                      (<= (count (:aka (d/entity udb (:e datom)))) 1)
                                      ;; or aka longer that 4 chars
                                      (>= (count (:v datom)) 4)))]

    (are [_db _res] (= (d/q '[:find ?v :where [_ :password ?v]] _db) _res)
      db                        #{["<SECRET>"] ["<PROTECTED>"] ["<UNKWOWN>"]}
      (d/filter db remove-pass) #{}
      (d/filter db remove-ivan) #{["<SECRET>"] ["<UNKWOWN>"]}
      (-> db (d/filter remove-ivan) (d/filter remove-pass)) #{})

    (are [_db _res] (= (d/q '[:find ?v :where [_ :aka ?v]] _db) _res)
      db                        #{["I"] ["Great"] ["Terrible"] ["IV"] ["II"]}
      (d/filter db remove-pass) #{["I"] ["Great"] ["Terrible"] ["IV"] ["II"]}
      (d/filter db remove-ivan) #{["I"] ["Great"] ["II"]}
      (d/filter db long-akas)   #{["Great"] ["Terrible"] ["II"]}
      (-> db (d/filter remove-ivan) (d/filter long-akas)) #{["Great"] ["II"]}
      (-> db (d/filter long-akas) (d/filter remove-ivan)) #{["Great"] ["II"]})

    (testing "Entities"
      (is (= (:password (d/entity db petr)) "<SECRET>"))
      (is (= (:password (d/entity (d/filter db remove-pass) petr) ::not-found) ::not-found))
      (is (= (:aka (d/entity db ivan)) #{"Terrible" "IV"}))
      (is (= (:aka (d/entity (d/filter db long-akas) ivan)) #{"Terrible"})))

    (testing "Index access"
      ;; With UUID-based entity IDs, order in AEVT index is not predictable, so use sets
      (is (= (set (map :v (d/datoms db :aevt :password)))
            #{"<SECRET>" "<PROTECTED>" "<UNKWOWN>"}))
      (is (= (map :v (d/datoms (d/filter db remove-pass) :aevt :password))
            [])))

    (testing "hash and equality"
      ;; content-based hashing was removed together with `hash-db`:
      ;; db values (filtered or not) compare by reference identity
      (let [filtered (d/filter db remove-ivan)]
        (is (= filtered filtered))
        (is (= (hash filtered) (hash filtered)))
        (is (not= filtered db))
        ;; a fresh `filter` call creates a new predicate closure
        (is (not= filtered (d/filter db remove-ivan))))))

  (testing "double filtering"
    (let [tx       (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
                     [{:db/id "petr", :name "Petr", :age 32}
                      {:db/id "oleg", :name "Oleg"}
                      {:db/id "ivan", :name "Ivan", :age 12}])
          db       (:db-after tx)
          has-age? (fn [db datom] (some? (:age (d/entity db (:e datom)))))
          adult?   (fn [db datom] (>= (:age (d/entity db (:e datom))) 18))
          names    (fn [db] (map :v (d/datoms db :aevt :name)))]
      (is (= ["Ivan" "Oleg" "Petr"] (sort (names db))))
      (is (= ["Ivan" "Petr"]        (sort (names (-> db (d/filter has-age?))))))
      (is (= ["Petr"]               (sort (names (-> db (d/filter has-age?) (d/filter adult?)))))))))
