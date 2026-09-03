(ns dbval.test.deref-value
  "Tests for deref value types: attributes flagged with {:dbval/deref true}
   store only a content hash in the index keys, the value bytes live in the
   store's blob area and reads surface BlobRefs (see `dbval.db/BlobRef`)."
  (:require
    [clojure.test :as t :refer [is deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.store :as store]
    [dbval.store.memory :as memory]))

(def schema
  {:doc/name  {:db/unique :db.unique/identity}
   :doc/model {:dbval/deref true}
   :doc/tags  {:dbval/deref true
               :db/cardinality :db.cardinality/many}})

(defn- conn []
  (d/conn-from-db (d/empty-db schema {:store (memory/store)})))

(def model-v1
  {:objects (mapv (fn [i] {:id i :content (apply str (repeat 50 "x"))})
                  (range 100))})

(def model-v2
  (assoc model-v1 :version 2))

(deftest test-roundtrip
  (let [conn (conn)]
    (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
    (let [v (d/q '[:find ?v . :where [_ :doc/model ?v]] @conn)]
      (is (db/blob-ref? v))
      (is (not (realized? v)))
      (is (= model-v1 @v))
      (is (realized? v)))
    (testing "entity api"
      (is (= model-v1 @(:doc/model (d/entity @conn [:doc/name "a"])))))
    (testing "pull api"
      (is (= model-v1 @(:doc/model (d/pull @conn [:doc/model] [:doc/name "a"])))))
    (testing "printing never fetches"
      (let [v (d/q '[:find ?v . :where [_ :doc/model ?v]] @conn)]
        (is (re-matches #"#dbval/blob-ref \"[0-9a-f]{64}\"" (pr-str v)))
        (is (not (realized? v)))))))

(deftest test-no-op-and-update
  (let [conn (conn)]
    (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
    (testing "re-asserting the same value is a no-op"
      (let [report (d/transact! conn [{:doc/name "a" :doc/model model-v1}])]
        (is (empty? (:tx-data report)))))
    (testing "a changed value retracts the old datom and asserts the new one"
      (let [report (d/transact! conn [{:doc/name "a" :doc/model model-v2}])
            model-datoms (filter #(= :doc/model (:a %)) (:tx-data report))]
        (is (= [false true] (mapv :added model-datoms)))
        (is (= model-v2 @(d/q '[:find ?v . :where [_ :doc/model ?v]] @conn)))))
    (testing "history keeps both versions"
      (is (= #{[model-v1 true] [model-v1 false] [model-v2 true]}
             (into #{}
                   (map (fn [datom] [@(:v datom) (:added datom)]))
                   (filter #(= :doc/model (:a %))
                           (d/datoms (d/history @conn) :eavt))))))))

(deftest test-retract
  (testing "retract by plain value"
    (let [conn (conn)]
      (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
      (d/transact! conn [[:db/retract [:doc/name "a"] :doc/model model-v1]])
      (is (nil? (d/q '[:find ?v . :where [_ :doc/model ?v]] @conn)))))
  (testing "retract by BlobRef"
    (let [conn (conn)]
      (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
      (let [v (d/q '[:find ?v . :where [_ :doc/model ?v]] @conn)]
        (d/transact! conn [[:db/retract [:doc/name "a"] :doc/model v]])
        (is (not (realized? v)))
        (is (nil? (d/q '[:find ?v . :where [_ :doc/model ?v]] @conn))))))
  (testing "retract-entity retracts deref datoms"
    (let [conn (conn)]
      (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
      (d/transact! conn [[:db/retractEntity [:doc/name "a"]]])
      (is (nil? (d/q '[:find ?v . :where [_ :doc/model ?v]] @conn))))))

(deftest test-copy-without-fetch
  (let [conn (conn)]
    (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
    (let [v (d/q '[:find ?v . :where [_ :doc/model ?v]] @conn)]
      (d/transact! conn [{:doc/name "b" :doc/model v}])
      (is (not (realized? v)))
      (is (= model-v1 @(:doc/model (d/entity @conn [:doc/name "b"])))))))

(deftest test-equality-join
  (let [conn (conn)]
    (d/transact! conn [{:doc/name "a" :doc/model model-v1}
                       {:doc/name "b" :doc/model model-v1}
                       {:doc/name "c" :doc/model model-v2}])
    (is (= #{["a" "b"] ["b" "a"]}
           (d/q '[:find ?n1 ?n2
                  :where
                  [?e1 :doc/model ?v]
                  [?e2 :doc/model ?v]
                  [(not= ?e1 ?e2)]
                  [?e1 :doc/name ?n1]
                  [?e2 :doc/name ?n2]]
                @conn)))))

(deftest test-unique-upsert
  (let [schema {:doc/id    {:db/unique :db.unique/identity
                            :dbval/deref true}
                :doc/count {}}
        conn (d/conn-from-db (d/empty-db schema {:store (memory/store)}))]
    (d/transact! conn [{:doc/id model-v1 :doc/count 1}])
    (testing "upsert by deref identity value resolves to the same entity"
      (d/transact! conn [{:doc/id model-v1 :doc/count 2}])
      (is (= [2] (d/q '[:find [?c ...] :where [_ :doc/count ?c]] @conn))))
    (testing "different value creates a new entity"
      (d/transact! conn [{:doc/id model-v2 :doc/count 3}])
      (is (= #{2 3} (set (d/q '[:find [?c ...] :where [_ :doc/count ?c]] @conn)))))
    (testing "lookup ref by deref value"
      (is (= 2 (:doc/count (d/entity @conn [:doc/id model-v1])))))))

(deftest test-cardinality-many
  (let [conn (conn)]
    (d/transact! conn [{:doc/name "a" :doc/tags [model-v1 model-v2]}])
    (is (= #{model-v1 model-v2}
           (into #{} (map deref) (d/q '[:find [?v ...] :where [_ :doc/tags ?v]] @conn))))
    (testing "re-asserting an existing value is a no-op"
      (let [report (d/transact! conn [[:db/add [:doc/name "a"] :doc/tags model-v1]])]
        (is (empty? (:tx-data report)))))
    (testing "retracting one value keeps the other"
      (d/transact! conn [[:db/retract [:doc/name "a"] :doc/tags model-v1]])
      (is (= #{model-v2}
             (into #{} (map deref) (d/q '[:find [?v ...] :where [_ :doc/tags ?v]] @conn)))))))

(deftest test-cas
  (let [conn (conn)]
    (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
    (let [e (:db/id (d/entity @conn [:doc/name "a"]))]
      (d/transact! conn [[:db/cas e :doc/model model-v1 model-v2]])
      (is (= model-v2 @(:doc/model (d/entity @conn [:doc/name "a"]))))
      (is (thrown? clojure.lang.ExceptionInfo
            (d/transact! conn [[:db/cas e :doc/model model-v1 model-v2]]))))))

(deftest test-tx-fn-read-your-writes
  (let [conn (conn)
        tx-fn (fn [db]
                ;; derefs a value asserted earlier in the same transaction:
                ;; must be served from the pending blob overlay
                (let [v (:doc/model (d/entity db [:doc/name "a"]))]
                  [{:doc/name "copy" :doc/model (assoc @v :copied true)}]))]
    (d/transact! conn [{:doc/name "a" :doc/model model-v1}
                       [:db.fn/call tx-fn]])
    (is (= (assoc model-v1 :copied true)
           @(:doc/model (d/entity @conn [:doc/name "copy"]))))))

(deftest test-dry-run
  (let [conn (conn)]
    (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
    (let [report  (d/with-dry-run @conn [{:doc/name "b" :doc/model model-v2}])
          report' (d/with-dry-run (:db-after report) [{:doc/name "c" :doc/model model-v1}])]
      (testing "speculative deref values are readable without a commit"
        (is (= model-v2 @(:doc/model (d/entity (:db-after report) [:doc/name "b"]))))
        (is (= model-v1 @(:doc/model (d/entity (:db-after report') [:doc/name "c"])))))
      (testing "nothing was committed"
        (is (nil? (d/entity @conn [:doc/name "b"])))))))

(deftest test-legacy-inline-datoms
  ;; datoms written before their attribute was flagged as deref keep the
  ;; serialized value inline in the index keys; flipping the schema flag must
  ;; not require a data migration
  (let [store    (memory/store)
        inline   (d/conn-from-db (d/empty-db {:doc/name {:db/unique :db.unique/identity}}
                                             {:store store}))
        _        (d/transact! inline [{:doc/name "a" :doc/model model-v1}])
        conn     (d/conn-from-db (d/empty-db schema {:store store}))]
    (testing "legacy datom reads as a realized-on-demand BlobRef"
      (let [v (:doc/model (d/entity @conn [:doc/name "a"]))]
        (is (db/blob-ref? v))
        (is (= model-v1 @v))))
    (testing "re-asserting the same value is a no-op across representations"
      (let [report (d/transact! conn [{:doc/name "a" :doc/model model-v1}])]
        (is (empty? (:tx-data report)))))
    (testing "updating a legacy datom retracts it"
      (d/transact! conn [{:doc/name "a" :doc/model model-v2}])
      (is (= model-v2 @(:doc/model (d/entity @conn [:doc/name "a"]))))
      (is (= 1 (count (vec (d/datoms @conn :aevt :doc/model))))))
    (testing "retract by value finds a legacy datom"
      (let [store  (memory/store)
            inline (d/conn-from-db (d/empty-db {:doc/name {:db/unique :db.unique/identity}}
                                               {:store store}))
            _      (d/transact! inline [{:doc/name "b" :doc/model model-v1}])
            conn   (d/conn-from-db (d/empty-db schema {:store store}))]
        (d/transact! conn [[:db/retract [:doc/name "b"] :doc/model model-v1]])
        (is (nil? (:doc/model (d/entity @conn [:doc/name "b"]))))))))

(deftest test-oversize-guard
  (let [conn (conn)
        huge (apply str (repeat 70000 "x"))]
    (testing "a large value on a non-deref attribute throws a descriptive error"
      (is (thrown? clojure.lang.ExceptionInfo
            (d/transact! conn [{:doc/name "a" :doc/plain huge}])))
      (try
        (d/transact! conn [{:doc/name "a" :doc/plain huge}])
        (catch clojure.lang.ExceptionInfo e
          (is (= :transact/value-too-large (:error (ex-data e)))))))
    (testing "the same value on a deref attribute works"
      (d/transact! conn [{:doc/name "a" :doc/model huge}])
      (is (= huge @(:doc/model (d/entity @conn [:doc/name "a"])))))))

(deftest test-sqlite-persistence
  (let [db-file (str (System/getProperty "java.io.tmpdir")
                     "/dbval-deref-test-" (random-uuid) ".db")]
    (let [conn (d/conn-from-db (d/empty-db schema {:db-file db-file}))]
      (d/transact! conn [{:doc/name "a" :doc/model model-v1}])
      (store/close! (db/db-store @conn)))
    (let [conn (d/conn-from-db (d/empty-db schema {:db-file db-file}))]
      (is (= model-v1 @(:doc/model (d/entity @conn [:doc/name "a"]))))
      (store/close! (db/db-store @conn)))))

(deftest test-bigdec-scale-variants
  ;; deref attributes hash the canonical representative, giving them the
  ;; same numeric equality the byte encoding gives inline attributes:
  ;; 0.50M finds, deduplicates and retracts against a stored 0.5M
  (let [conn (conn)]
    (d/transact! conn [{:doc/name "a" :doc/model 0.5M}
                       {:doc/name "b" :doc/model 0.50M}])
    (let [ea (:db/id (d/entity @conn [:doc/name "a"]))
          eb (:db/id (d/entity @conn [:doc/name "b"]))]
      (testing "an index lookup with the other scale finds the datom"
        (is (= 1 (count (vec (d/datoms @conn :eavt ea :doc/model 0.50M))))))
      (testing "both scales hash to the same blob"
        (is (= (:doc/model (d/entity @conn ea))
               (:doc/model (d/entity @conn eb)))))
      (testing "retracting with the other scale removes the datom"
        (d/transact! conn [[:db/retract ea :doc/model 0.50M]])
        (is (empty? (vec (d/datoms @conn :eavt ea :doc/model))))))))
