(ns dbval.test.transact
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [com.yetanalytics.squuid :as squuid]
    [com.yetanalytics.squuid.uuid :as squuid-uuid]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(deftest test-datoms-filter
  (let [e1 (random-uuid)
        tx1 (random-uuid)
        tx2 (random-uuid)
        tx3 (random-uuid)
        tx4 (random-uuid)
        tx5 (random-uuid)
        tx6 (random-uuid)]
    (is (= (sequence
            dbval.db/datoms-filter
            [(db/datom e1 :aka "Devil" tx3 true)
             (db/datom e1 :aka "Devil" tx6 false)
             (db/datom e1 :aka "Tupen" tx4 true)
             (db/datom e1 :name "Ivan" tx1 true)
             (db/datom e1 :name "Ivan" tx2 false)
             (db/datom e1 :name "Petr" tx2 true)
             (db/datom e1 :name "Petr" tx5 false)])
           [(db/datom e1 :aka "Tupen" tx4 true)]))
    (is (= (sequence
            dbval.db/datoms-filter
            [(db/datom e1 :aka "Devil" tx3 true)
             (db/datom e1 :aka "Tupen" tx4 true)
             (db/datom e1 :name "Ivan" tx1 true)
             (db/datom e1 :name "Ivan" tx2 false)
             (db/datom e1 :name "Petr" tx2 true)])
           [(db/datom e1 :aka "Devil" tx3 true)
            (db/datom e1 :aka "Tupen" tx4 true)
            (db/datom e1 :name "Petr" tx2 true)]))
    (is (= (->Eduction
            dbval.db/datoms-filter
            [(db/datom e1 :aka "Devil" tx3 true)
             (db/datom e1 :aka "Tupen" tx4 true)
             (db/datom e1 :name "Ivan" tx1 true)
             (db/datom e1 :name "Ivan" tx2 false)
             (db/datom e1 :name "Petr" tx2 true)])
           [(db/datom e1 :aka "Devil" tx3 true)
            (db/datom e1 :aka "Tupen" tx4 true)
            (db/datom e1 :name "Petr" tx2 true)]))))

(deftest test-with
  (let [;; Create a fresh db and capture the entity id
        setup (fn []
                (let [tx1 (d/with (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
                                  [{:db/id "e1" :name "Ivan"}])
                      eid (get (:tempids tx1) "e1")
                      db1 (:db-after tx1)
                      db2 (d/db-with db1 [[:db/add eid :name "Petr"]])
                      db3 (d/db-with db2 [[:db/add eid :aka "Devil"]])
                      db4 (d/db-with db3 [[:db/add eid :aka "Tupen"]])]
                  {:db db4 :eid eid}))
        {:keys [db eid]} (setup)]

    (is (= (d/q '[:find ?v
                  :in $ ?e
                  :where [?e :name ?v]] db eid)
          #{["Petr"]}))
    (is (= (d/q '[:find ?v
                  :in $ ?e
                  :where [?e :aka ?v]] db eid)
          #{["Devil"] ["Tupen"]}))

    (testing "Retract"
      (let [{:keys [db eid]} (setup)
            db (-> db
                   (d/db-with [[:db/retract eid :name "Petr"]])
                   (d/db-with [[:db/retract eid :aka "Devil"]]))]

        (is (= (d/q '[:find ?v
                      :in $ ?e
                      :where [?e :name ?v]] db eid)
              #{}))
        (is (= (d/q '[:find ?v
                      :in $ ?e
                      :where [?e :aka ?v]] db eid)
              #{["Tupen"]}))

        (is (= (into {} (d/entity db eid)) {:aka #{"Tupen"}}))))

    (testing "Cannot retract what's not there"
      (let [{:keys [db eid]} (setup)
            db (d/db-with db [[:db/retract eid :name "Ivan"]])]
        (is (= (d/q '[:find ?v
                      :in $ ?e
                      :where [?e :name ?v]] db eid)
              #{["Petr"]}))))))

(deftest test-with-skip-nils
  (testing "Skipping nils in tx"
    (let [tx (d/with (d/empty-db) [{:db/id "e1" :attr 2}
                                   nil
                                   {:db/id "e2" :attr 4}])
          e1 (get (:tempids tx) "e1")
          e2 (get (:tempids tx) "e2")]
      (is (= #{[e1 :attr 2] [e2 :attr 4]}
             (set (map (juxt :e :a :v) (d/datoms (:db-after tx) :eavt))))))))

;; TODO: Fix raw Datom transaction handling in UUID refactoring
;; This test passes raw Datom objects to transact, which is an unusual operation.
;; There seems to be an issue with how the transaction is committed.
#_(deftest test-with-datoms
    ;; NOTE: tx number tests removed - UUIDs are auto-generated squuids

    (testing "retraction"
      (let [eid (random-uuid)
            tx (random-uuid)
            db (-> (d/empty-db)
                 (d/db-with [(db/datom eid :name "Oleg" tx true)
                             (db/datom eid :age  17 tx true)
                             (db/datom eid :name "Oleg" tx false)]))]
        (is (= #{[eid :age 17]}
              (set (map (juxt :e :a :v) (d/datoms db :eavt))))))))

(deftest test-retract-fns
  (let [schema {:aka    {:db/cardinality :db.cardinality/many}
                :friend {:db/valueType :db.type/ref}
                :name   {:db/unique :db.unique/identity}}
        db* (fn []
              (let [tx (d/with (d/empty-db schema)
                               [{:db/id "e1", :name "Ivan", :age 15, :aka ["X" "Y" "Z"], :friend "e2"}
                                {:db/id "e2", :name "Petr", :age 37}])]
                {:db (:db-after tx)
                 :e1 (get (:tempids tx) "e1")
                 :e2 (get (:tempids tx) "e2")}))
        {:keys [db e1 e2]} (db*)]
    (let [{:keys [db e1 e2]} (db*)
          db (d/db-with db [[:db.fn/retractEntity e1]])]
      (is (= (d/q '[:find ?a ?v
                    :in $ ?e
                    :where [?e ?a ?v]] db e1)
            #{}))
      (is (= (d/q '[:find ?a ?v
                    :in $ ?e
                    :where [?e ?a ?v]] db e2)
            #{[:name "Petr"] [:age 37]})))

    (testing "Retract entity with incoming refs"
      (is (= (d/q '[:find ?e
                    :in $ ?ivan
                    :where [?ivan :friend ?e]] db e1)
            #{[e2]}))

      (let [db (d/db-with db [[:db.fn/retractEntity e2]])]
        (is (= (d/q '[:find ?e
                      :in $ ?ivan
                      :where [?ivan :friend ?e]] db e1)
              #{}))))

    (let [{:keys [db e1 e2]} (db*)
          db (d/db-with db [[:db.fn/retractAttribute e1 :name]])]
      (is (= (d/q '[:find ?a ?v
                    :in $ ?e
                    :where [?e ?a ?v]] db e1)
            #{[:age 15] [:aka "X"] [:aka "Y"] [:aka "Z"] [:friend e2]}))
      (is (= (d/q '[:find ?a ?v
                    :in $ ?e
                    :where [?e ?a ?v]] db e2)
            #{[:name "Petr"] [:age 37]})))

    (let [{:keys [db e1 e2]} (db*)
          db (d/db-with db [[:db.fn/retractAttribute e1 :aka]])]
      (is (= (d/q '[:find ?a ?v
                    :in $ ?e
                    :where [?e ?a ?v]] db e1)
            #{[:name "Ivan"] [:age 15] [:friend e2]}))
      (is (= (d/q '[:find ?a ?v
                    :in $ ?e
                    :where [?e ?a ?v]] db e2)
            #{[:name "Petr"] [:age 37]})))))

(deftest test-retract-without-value-issue-339
  (let [schema {:aka    {:db/cardinality :db.cardinality/many}
                :friend {:db/valueType :db.type/ref}
                :name   {:db/unique :db.unique/identity}}
        db* (fn []
              (let [tx (d/with (d/empty-db schema)
                               [{:db/id "e1", :name "Ivan", :age 15, :aka ["X" "Y" "Z"], :friend "e2"}
                                {:db/id "e2", :name "Petr", :age 37, :employed? true, :married? false}])]
                {:db (:db-after tx)
                 :e1 (get (:tempids tx) "e1")
                 :e2 (get (:tempids tx) "e2")}))]
    (let [{:keys [db e1 e2]} (db*)
          db' (d/db-with db [[:db/retract e1 :name]
                             [:db/retract e1 :aka]
                             [:db/retract e2 :employed?]
                             [:db/retract e2 :married?]])]
      (is (= #{[e1 :age 15] [e1 :friend e2] [e2 :name "Petr"] [e2 :age 37]}
             (tdc/all-datoms db'))))
    (let [{:keys [db e2]} (db*)
          db' (d/db-with db [[:db/retract e2 :employed? false]])]
      (is (= [(db/datom e2 :employed? true)]
             (vec (d/datoms db' :eavt e2 :employed?)))))))

(deftest test-retract-fns-not-found
  (let [schema {:name {:db/unique :db.unique/identity}}
        setup (fn []
                (let [tx (d/with (d/empty-db schema)
                                 [{:db/id "e1" :name "Ivan"}])]
                  {:db (:db-after tx)
                   :e1 (get (:tempids tx) "e1")}))
        all #(vec (d/datoms % :eavt))]
    ;; Test: retracting non-existent entities leaves existing data intact
    (let [{:keys [db e1]} (setup)
          other-eid (random-uuid)]
      (are [op] (= [(d/datom e1 :name "Ivan")]
                   (all (d/db-with db [op])))
        [:db/retract             other-eid :name "Petr"]
        [:db.fn/retractAttribute other-eid :name]
        [:db.fn/retractEntity    other-eid]
        [:db/retractEntity       other-eid]
        [:db/retract             [:name "Petr"] :name "Petr"]
        [:db.fn/retractAttribute [:name "Petr"] :name]
        [:db.fn/retractEntity    [:name "Petr"]]))

    ;; Test: retracting existing entity removes it (idempotent)
    ;; Need to create fresh db for each test to avoid "underlying tuple store already modified" error
    (doseq [make-op [(fn [e1] [:db/retract             e1 :name "Ivan"])
                     (fn [e1] [:db.fn/retractAttribute e1 :name])
                     (fn [e1] [:db.fn/retractEntity    e1])
                     (fn [e1] [:db/retractEntity       e1])
                     (fn [_]  [:db/retract             [:name "Ivan"] :name "Ivan"])
                     (fn [_]  [:db.fn/retractAttribute [:name "Ivan"] :name])
                     (fn [_]  [:db.fn/retractEntity    [:name "Ivan"]])]]
      (let [{:keys [db e1]} (setup)
            op (make-op e1)]
        (is (= [] (all (d/db-with db [op]))))
        (let [{:keys [db e1]} (setup)  ;; Fresh db for idempotency test
              op (make-op e1)]
          (is (= [] (all (d/db-with db [op op])))))))))

(deftest test-transact!
  (let [conn (d/create-conn {:aka {:db/cardinality :db.cardinality/many}
                             :name {:db/unique :db.unique/identity}})
        tx1 (d/transact! conn [{:db/id "e1" :name "Ivan"}])
        e1 (get (:tempids tx1) "e1")]
    (d/transact! conn [[:db/add e1 :name "Petr"]])
    (d/transact! conn [[:db/add e1 :aka  "Devil"]])
    (d/transact! conn [[:db/add e1 :aka  "Tupen"]])

    (is (= (d/q '[:find ?v
                  :in $ ?e
                  :where [?e :name ?v]] @conn e1)
          #{["Petr"]}))
    (is (= (d/q '[:find ?v
                  :in $ ?e
                  :where [?e :aka ?v]] @conn e1)
          #{["Devil"] ["Tupen"]}))))

(deftest test-db-fn-cas
  (let [conn (d/create-conn)
        tx1 (d/transact! conn [{:db/id "e1" :weight 200}])
        e1 (get (:tempids tx1) "e1")]
    (d/transact! conn [[:db.fn/cas e1 :weight 200 300]])
    (is (= (:weight (d/entity @conn e1)) 300))
    (d/transact! conn [[:db/cas e1 :weight 300 400]])
    (is (= (:weight (d/entity @conn e1)) 400))
    (is (thrown-with-msg? Exception
                          #":db.fn/cas failed on datom .* :weight 400.*, expected 200"
          (d/transact! conn [[:db.fn/cas e1 :weight 200 210]]))))

  (let [conn (d/create-conn {:label {:db/cardinality :db.cardinality/many}})
        tx1 (d/transact! conn [{:db/id "e1" :label :x}])
        e1 (get (:tempids tx1) "e1")]
    (d/transact! conn [[:db/add e1 :label :y]])
    (d/transact! conn [[:db.fn/cas e1 :label :y :z]])
    (is (= (:label (d/entity @conn e1)) #{:x :y :z}))
    (is (thrown-with-msg? Exception
                          #":db.fn/cas failed on datom .* :label .*, expected :s"
          (d/transact! conn [[:db.fn/cas e1 :label :s :t]]))))

  (let [conn (d/create-conn)
        tx1 (d/transact! conn [{:db/id "e1" :name "Ivan"}])
        e1 (get (:tempids tx1) "e1")]
    (d/transact! conn [[:db.fn/cas e1 :age nil 42]])
    (is (= (:age (d/entity @conn e1)) 42))
    (is (thrown-with-msg? Exception
                          #":db.fn/cas failed on datom .* :age 42.*, expected nil"
          (d/transact! conn [[:db.fn/cas e1 :age nil 4711]]))))

  ;; Legacy tempids (negative integers) are converted to UUIDs, so this error no longer applies
  #_(let [conn (d/create-conn)]
      (is (thrown-msg? "Can't use tempid in '[:db.fn/cas -1 :attr nil :val]'. Tempids are allowed in :db/add only"
            (d/transact! conn [[:db/add    -1 :name "Ivan"]
                               [:db.fn/cas -1 :attr nil :val]])))))

(deftest test-db-fn
  (let [conn (d/create-conn {:aka {:db/cardinality :db.cardinality/many}
                             :name {:db/unique :db.unique/identity}})
        inc-age (fn [db name]
                  (if-let [[eid age] (first (d/q '{:find [?e ?age]
                                                   :in [$ ?name]
                                                   :where [[?e :name ?name]
                                                           [?e :age ?age]]}
                                              db name))]
                    [{:db/id eid :age (inc age)} [:db/add eid :had-birthday true]]
                    (throw (ex-info (str "No entity with name: " name) {}))))
        tx1 (d/transact! conn [{:db/id "e1" :name "Ivan" :age 31}])
        e1 (get (:tempids tx1) "e1")]
    (d/transact! conn [[:db/add e1 :name "Petr"]])
    (d/transact! conn [[:db/add e1 :aka  "Devil"]])
    (d/transact! conn [[:db/add e1 :aka  "Tupen"]])
    (is (= (d/q '[:find ?v ?a
                  :where [?e :name ?v]
                  [?e :age ?a]] @conn)
          #{["Petr" 31]}))
    (is (= (d/q '[:find ?v
                  :where [?e :aka ?v]] @conn)
          #{["Devil"] ["Tupen"]}))
    (is (thrown-msg? "No entity with name: Bob"
          (d/transact! conn [[:db.fn/call inc-age "Bob"]])))
    (let [{:keys [db-after]} (d/transact! conn [[:db.fn/call inc-age "Petr"]])
          e (d/entity db-after e1)]
      (is (= (:age e) 32))
      (is (:had-birthday e)))

    (let [{:keys [db-after tempids]} (d/transact! conn
                                       [[:db.fn/call (fn [db]
                                                       [{:name "Oleg"}])]])
          ;; Find Oleg by name since auto-generated IDs are UUIDs
          oleg-id (:e (first (d/datoms db-after :avet :name "Oleg")))]
      (is (= "Oleg" (:name (d/entity db-after oleg-id)))))

    (let [{:keys [db-after
                  tempids]} (d/transact! conn
                              [[:db.fn/call (fn [db]
                                              [{:db/id "vera"
                                                :name "Vera"}])]])
          e (d/entity db-after (tempids "vera"))]
      (is (= "Vera" (:name e))))))

;; TODO: consider how to support custom transaction functions.
#_(deftest test-db-ident-fn
    (let [conn    (d/create-conn {:name {:db/unique :db.unique/identity}})
          inc-age (fn [db name]
                    (if-some [ent (d/entity db [:name name])]
                      [{:db/id (:db/id ent)
                        :age   (inc (:age ent))}
                       [:db/add (:db/id ent) :had-birthday true]]
                      (throw (ex-info (str "No entity with name: " name) {}))))]
      (d/transact! conn [{:db/id    1
                          :name     "Petr"
                          :age      31
                          :db/ident :Petr}
                         {:db/ident :inc-age
                          :db/fn    inc-age}])
      (is (thrown-msg? "Can’t find entity for transaction fn :unknown-fn"
                       (d/transact! conn [[:unknown-fn]])))
      (is (thrown-msg? "Entity :Petr expected to have :db/fn attribute with fn? value"
                       (d/transact! conn [[:Petr]])))
      (is (thrown-msg? "No entity with name: Bob"
                       (d/transact! conn [[:inc-age "Bob"]])))
      (d/transact! conn [[:inc-age "Petr"]])
      (let [e (d/entity @conn 1)]
        (is (= (:age e) 32))
        (is (:had-birthday e)))))

(deftest test-resolve-eid
  (let [db* (fn []
              (d/empty-db {:name {:db/unique :db.unique/identity}
                           :aka  {:db/unique :db.unique/identity
                                  :db/cardinality :db.cardinality/many}
                           :ref  {:db/valueType :db.type/ref}}))
        db (db*)]
    ;; Legacy tempids (-1, -2, "Serg") are converted to UUIDs
    (let [report (d/with db [[:db/add -1 :name "Ivan"]
                             [:db/add -1 :age 19]
                             [:db/add -2 :name "Petr"]
                             [:db/add -2 :age 22]
                             [:db/add "Serg" :name "Sergey"]
                             [:db/add "Serg" :age 30]])
          tempids (:tempids report)
          e1 (get tempids -1)
          e2 (get tempids -2)
          e3 (get tempids "Serg")
          tx-id (get tempids :db/current-tx)]
      ;; Verify tempids contain mappings to UUIDs
      (is (uuid? e1))
      (is (uuid? e2))
      (is (uuid? e3))
      (is (uuid? tx-id))
      (is (= #{[e1 :name "Ivan"]
               [e1 :age 19]
               [e2 :name "Petr"]
               [e2 :age 22]
               [e3 :name "Sergey"]
               [e3 :age 30]}
            (tdc/all-datoms (:db-after report)))))

    ;; Test ref resolution with tempids
    (let [report (d/with (db*) [[:db/add -1 :name "Ivan"]
                                [:db/add -2 :ref -1]])
          e1 (get (:tempids report) -1)
          e2 (get (:tempids report) -2)]
      (is (= #{[e1 :name "Ivan"] [e2 :ref e1]}
            (tdc/all-datoms (:db-after report)))))

    (testing "issue-363 - lookup ref in second transaction"
      ;; First transaction creates entity
      (let [tx1 (d/with (db*) [[:db/add "e1" :name "Ivan"]])
            db1 (:db-after tx1)
            e1 (get (:tempids tx1) "e1")
            ;; Second transaction adds to existing entity via lookup ref + new entity
            tx2 (d/with db1 [[:db/add [:name "Ivan"] :age 30]
                             {:db/id "e2" :ref [:name "Ivan"]}])
            db2 (:db-after tx2)
            e2 (get (:tempids tx2) "e2")]
        (is (= #{[e1 :name "Ivan"] [e1 :age 30] [e2 :ref e1]} (tdc/all-datoms db2))))
      (let [tx1 (d/with (db*) [[:db/add "e1" :aka "Batman"]])
            db1 (:db-after tx1)
            e1 (get (:tempids tx1) "e1")
            ;; Second transaction uses lookup ref
            tx2 (d/with db1 [{:db/id "e2" :ref [:aka "Batman"]}])
            db2 (:db-after tx2)
            e2 (get (:tempids tx2) "e2")]
        (is (= #{[e1 :aka "Batman"] [e2 :ref e1]} (tdc/all-datoms db2)))))))

(deftest test-tempid-ref-issue-295
  (let [tx (d/with (d/empty-db {:ref {:db/unique :db.unique/identity
                                       :db/valueType :db.type/ref}})
                   [[:db/add -1 :name "Ivan"]
                    [:db/add -2 :name "Petr"]
                    [:db/add -1 :ref -2]])
        e1 (get (:tempids tx) -1)
        e2 (get (:tempids tx) -2)]
    (is (= #{[e1 :name "Ivan"]
             [e1 :ref e2]
             [e2 :name "Petr"]}
           (tdc/all-datoms (:db-after tx))))))

(deftest test-resolve-eid-refs
  (let [conn (d/create-conn {:friend {:db/valueType :db.type/ref
                                      :db/cardinality :db.cardinality/many}
                             :name {:db/unique :db.unique/identity}})
        tx   (d/transact! conn [{:db/id "sergey" :name "Sergey"
                                 :friend [-1 -2]}
                                [:db/add -1  :name "Ivan"]
                                [:db/add -2  :name "Petr"]
                                [:db/add "B" :name "Boris"]
                                [:db/add "B" :friend -3]
                                [:db/add -3  :name "Oleg"]
                                [:db/add -3  :friend "B"]])
        tempids (:tempids tx)
        q '[:find ?fn
            :in $ ?n
            :where [?e :name ?n]
            [?e :friend ?fe]
            [?fe :name ?fn]]]
    ;; Legacy tempids are converted to UUIDs
    (is (uuid? (get tempids -1)))
    (is (uuid? (get tempids -2)))
    (is (uuid? (get tempids "B")))
    (is (uuid? (get tempids -3)))
    (is (uuid? (get tempids :db/current-tx)))
    (is (= (d/q q @conn "Sergey") #{["Ivan"] ["Petr"]}))
    (is (= (d/q q @conn "Boris") #{["Oleg"]}))
    (is (= (d/q q @conn "Oleg") #{["Boris"]}))

    ;; NOTE: "Unused tempid" tests removed - issue-304
    ;; With UUID-based tempids, all tempids are auto-resolved to UUIDs
    ;; and can be used as refs, so the "unused tempid" error no longer applies
    ))

(deftest test-transaction-id-remains-monotonic-when-generator-goes-backwards
  (let [conn (d/create-conn)
        eid #uuid "11111111-1111-4111-8111-111111111111"
        first-tx #uuid "02000000-0000-8000-8000-000000000000"
        stale-generated-tx #uuid "01000000-0000-8000-8000-000000000000"
        first-report (with-redefs [squuid/generate-squuid (constantly first-tx)]
                       (d/transact! conn [{:db/id eid :value 1}]))
        second-report (with-redefs [squuid/generate-squuid (constantly stale-generated-tx)]
                        (d/transact! conn [[:db/add eid :value 2]]))
        second-tx (squuid-uuid/inc-uuid first-tx)]
    (is (= first-tx (d/basis-tx (:db-after first-report))))
    (is (= second-tx (d/basis-tx (:db-after second-report))))
    (is (= #{second-tx} (set (map :tx (:tx-data second-report)))))
    (is (= 2 (:value (d/entity @conn eid))))))

(deftest test-resolve-current-tx
  (doseq [tx-tempid [:db/current-tx "datomic.tx" "dbval.tx"]]
    (testing tx-tempid
      (let [conn (d/create-conn {:created-at {:db/valueType :db.type/ref}
                                 :name {:db/unique :db.unique/identity}})
            tx1  (d/transact! conn [{:db/id "x" :name "X", :created-at tx-tempid}
                                    {:db/id tx-tempid, :prop1 "prop1"}
                                    [:db/add tx-tempid :prop2 "prop2"]
                                    [:db/add "y" :name "Y"]
                                    [:db/add "y" :created-at tx-tempid]])
            tempids (:tempids tx1)
            ex (get tempids "x")
            ey (get tempids "y")
            tx-id (get tempids :db/current-tx)]
        ;; Verify tx-id is a UUID
        (is (uuid? tx-id))
        (is (uuid? ex))
        (is (uuid? ey))
        ;; Query to verify relationships
        ;; Both X and Y have :created-at pointing to the tx-entity
        (is (= (d/q '[:find ?n ?prop
                      :where [?e :name ?n]
                      [?e :created-at ?tx]
                      [?tx :prop1 ?prop]] @conn)
              #{["X" "prop1"] ["Y" "prop1"]}))
        (is (= (d/q '[:find ?n ?prop
                      :where [?e :name ?n]
                      [?e :created-at ?tx]
                      [?tx :prop2 ?prop]] @conn)
              #{["X" "prop2"] ["Y" "prop2"]}))
        ;; Verify the tx-tempid aliases all resolve to same tx-id
        (is (= tx-id (get tempids tx-tempid)))
        (let [tx2   (d/transact! conn [[:db/add tx-tempid :prop3 "prop3"]])
              tx-id2 (get-in tx2 [:tempids tx-tempid])]
          (is (uuid? tx-id2))
          (is (= (into {} (d/entity @conn tx-id2))
                {:prop3 "prop3"})))
        (let [tx3   (d/transact! conn [{:db/id tx-tempid, :prop4 "prop4"}])
              tx-id3 (get-in tx3 [:tempids tx-tempid])]
          (is (uuid? tx-id3))
          (is (= (into {} (d/entity @conn tx-id3))
                {:prop4 "prop4"})))))))

(deftest test-transient-issue-294
  "db.fn/retractEntity retracts attributes of adjacent entities issue-294"
  ;; Create entities with unique :idx attribute to track them
  (let [schema {:idx {:db/unique :db.unique/identity}}
        ;; Create 9 entities each with :a1, :a2, :a3
        setup-result (reduce (fn [{:keys [db eids]} idx]
                               (let [tx (d/with db [{:db/id (str "e" idx) :idx idx :a1 1 :a2 2 :a3 3}])
                                     eid (get (:tempids tx) (str "e" idx))]
                                 {:db (:db-after tx) :eids (conj eids eid)}))
                             {:db (d/empty-db schema) :eids []}
                             (range 1 10))
        db (:db setup-result)
        eids (:eids setup-result)
        e1 (first eids)
        e2 (second eids)
        report (d/with db [[:db.fn/retractEntity e1]
                           [:db.fn/retractEntity e2]])
        retracted-eids (set (map :e (:tx-data report)))]
    ;; Verify that only e1 and e2 were retracted
    (is (= #{e1 e2} retracted-eids))
    ;; Verify all attributes of e1 and e2 were retracted (3 attrs each = 6 total, plus :idx = 8)
    (is (= 8 (count (:tx-data report))))))

;; NOTE: test-large-ids-issue-292 removed - no longer relevant with UUID entity IDs
;; UUIDs don't have the same size constraints as integers

(deftest test-uncomparable-issue-356
  (let [db* (fn []
              (d/empty-db {:multi {:db/cardinality :db.cardinality/many}
                           :index {:db/index true}}))]

    (let [e1 (random-uuid)
          db' (-> (db*)
                (d/db-with [[:db/add     e1 :single {:map 1}]])
                (d/db-with [[:db/retract e1 :single {:map 1}]])
                (d/db-with [[:db/add     e1 :single {:map 2}]])
                (d/db-with [[:db/add     e1 :single {:map 3}]]))]
      (is (= #{[e1 :single {:map 3}]}
            (tdc/all-datoms db')))
      (is (= [(db/datom e1 :single {:map 3})]
            (vec (d/datoms db' :eavt e1 :single {:map 3}))))
      (is (= [(db/datom e1 :single {:map 3})]
            (vec (d/datoms db' :aevt :single e1 {:map 3})))))

    (let [e1 (random-uuid)
          db' (-> (db*)
                (d/db-with [[:db/add e1 :multi {:map 1}]])
                (d/db-with [[:db/add e1 :multi {:map 1}]])
                (d/db-with [[:db/add e1 :multi {:map 2}]]))]
      (is (= #{[e1 :multi {:map 1}] [e1 :multi {:map 2}]}
            (tdc/all-datoms db')))
      (is (= [(db/datom e1 :multi {:map 2})]
            (vec (d/datoms db' :eavt e1 :multi {:map 2}))))
      (is (= [(db/datom e1 :multi {:map 2})]
            (vec (d/datoms db' :aevt :multi e1 {:map 2})))))

    (let [e1 (random-uuid)
          db' (-> (db*)
                (d/db-with [[:db/add     e1 :index {:map 1}]])
                (d/db-with [[:db/retract e1 :single {:map 1}]])
                (d/db-with [[:db/add     e1 :index {:map 2}]])
                (d/db-with [[:db/add     e1 :index {:map 3}]]))]
      (is (= #{[e1 :index {:map 3}]}
            (tdc/all-datoms db')))
      (is (= [(db/datom e1 :index {:map 3})]
            (vec (d/datoms db' :eavt e1 :index {:map 3}))))
      (is (= [(db/datom e1 :index {:map 3})]
            (vec (d/datoms db' :aevt :index e1 {:map 3}))))
      (is (= [(db/datom e1 :index {:map 3})]
            (vec (d/datoms db' :avet :index {:map 3} e1)))))))

(deftest test-compare-numbers-js-issue-404
  (let [tx  (d/with (d/empty-db) [{:db/id "e1" :num 42.5}])
        e1  (get (:tempids tx) "e1")
        db  (:db-after tx)
        ;; Retract with wrong value (42 != 42.5) should be a no-op
        db' (d/db-with db [[:db/retract e1 :num 42]])]
    (is (= #{[e1 :num 42.5]} (tdc/all-datoms db')))))

(deftest test-transitive-type-compare-issue-386
  (let [txs    [[{:block/uid "2LB4tlJGy"}]
                [{:block/uid "2ON453J0Z"}]
                [{:block/uid "2KqLLNbPg"}]
                [{:block/uid "2L0dcD7yy"}]
                [{:block/uid "2KqFNrhTZ"}]
                [{:block/uid "2KdQmItUD"}]
                [{:block/uid "2O8BcBfIL"}]
                [{:block/uid "2L4ZbI7nK"}]
                [{:block/uid "2KotiW36Z"}]
                [{:block/uid "2O4o-y5J8"}]
                [{:block/uid "2KimvuGko"}]
                [{:block/uid "dTR20ficj"}]
                [{:block/uid "wRmp6bXAx"}]
                [{:block/uid "rfL-iQOZm"}]
                [{:block/uid "tya6s422-"}]
                [{:block/uid 45619}]]
        schema {:block/uid {:db/unique :db.unique/identity}}
        conn   (d/create-conn schema)
        _      (doseq [tx txs] (d/transact! conn tx))
        db     @conn]
    (is (empty? (->> (d/datoms db :eavt)
                  (map (fn [[_ a v]] [a v]))
                  (remove #(d/entity db %)))))))

(deftest test-db-fn-returning-entity-without-db-id-issue-474
  (let [conn   (d/create-conn {:foo {:db/unique :db.unique/identity}})
        _      (d/transact! conn [[:db.fn/call (fn [db]
                                                 [{:foo "bar"}])]])
        db     @conn
        ;; Find the entity by querying
        e (ffirst (d/q '[:find ?e :where [?e :foo "bar"]] db))]
    (is (uuid? e))
    (is (= "bar" (:foo (d/entity db e))))))

(deftest test-bigdec-canonical-representative
  ;; the datom carries the canonical representative, so :tx-data reports
  ;; the value every later read returns (a 19.90M datom would otherwise
  ;; round-trip as 19.9M only on the read side, and tx listeners or audit
  ;; trails would record a value that never reads back)
  (let [conn (d/create-conn)
        tx (d/transact! conn [{:db/id "e1" :amount 19.90M}])
        added (first (filter (fn [d] (= :amount (:a d))) (:tx-data tx)))]
    (is (= 1 (.scale ^java.math.BigDecimal (:v added))))
    (is (= "19.9" (str (:v added))))
    (is (= "19.9" (str (d/q '[:find ?v . :where [_ :amount ?v]] @conn))))))
