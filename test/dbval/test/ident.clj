(ns dbval.test.ident
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]))

(def *db
  (delay
    (let [tx (d/with (d/empty-db {:ref {:db/valueType :db.type/ref}})
               [[:db/add "e1" :db/ident :ent1]
                [:db/add "e2" :db/ident :ent2]
                [:db/add "e2" :ref "e1"]])]
      {:db (:db-after tx)
       :e1 (get (:tempids tx) "e1")
       :e2 (get (:tempids tx) "e2")})))

(deftest test-q
  (let [{:keys [db e1 e2]} @*db]
    (is (= e1 (d/q '[:find ?v .
                     :where [:ent2 :ref ?v]] db)))
    (is (= e2 (d/q '[:find ?f .
                     :where [?f :ref :ent1]] db)))))

(deftest test-transact!
  (let [{:keys [db]} @*db
        db' (d/db-with db [[:db/add :ent1 :ref :ent2]])]
    ;; Datomic parity: a ref to an entity with a :db/ident resolves to the
    ;; ident keyword in the entity API.
    (is (= :ent2 (:ref (d/entity db' :ent1))))))

(deftest test-entity-ref-ident-resolution
  (let [tx (d/with (d/empty-db {:ref  {:db/valueType :db.type/ref}
                                :refs {:db/valueType :db.type/ref
                                       :db/cardinality :db.cardinality/many}})
             [[:db/add "enum" :db/ident :color/red]
              {:db/id "plain" :name "no ident"}
              {:db/id "e" :ref "enum" :refs ["enum" "plain"]}])
        db (:db-after tx)
        plain-id (get (:tempids tx) "plain")
        e-id (get (:tempids tx) "e")
        e (d/entity db e-id)]
    (is (= :color/red (:ref e)))
    (is (= #{:color/red (d/entity db plain-id)} (:refs e)))
    (testing "after touch"
      (let [e (d/touch (d/entity db e-id))]
        (is (= :color/red (:ref e)))))
    (testing "refs to entities without ident stay entities"
      (is (= plain-id (-> (:refs e) (disj :color/red) first :db/id))))))

(deftest test-entity
  (let [{:keys [db]} @*db]
    (is (= {:db/ident :ent1}
          (into {} (d/entity db :ent1))))))

(deftest test-pull
  (let [{:keys [db e1]} @*db]
    (is (= {:db/id e1, :db/ident :ent1}
          (d/pull db '[*] :ent1)))))
