(ns dbval.store.slatedb-test
  "Runs representative engine flows against the SlateDB store — the same
   flows dbval.test.store runs against the memory store. Uses the in-memory
   object store (\"memory:///\"), so nothing touches the filesystem."
  (:require
    [clojure.test :as t :refer [is deftest testing]]
    [dbval.core :as d]
    [dbval.store.slatedb :as slatedb]))

(defn- empty-slatedb-db
  ([] (empty-slatedb-db nil))
  ([schema]
   (d/empty-db schema {:store (slatedb/store {:object-store-url "memory:///"})})))

(deftest test-slatedb-store-transact-and-query
  (let [conn (d/conn-from-db
               (empty-slatedb-db {:name    {:db/unique :db.unique/identity}
                                  :aka     {:db/cardinality :db.cardinality/many}
                                  :friend  {:db/valueType :db.type/ref}
                                  :age     {:db/index true}}))]
    (d/transact! conn [{:db/id "ivan" :name "Ivan" :age 30 :aka ["I" "Terrible"]}
                       {:db/id "petr" :name "Petr" :age 44 :friend "ivan"}])
    (let [db   @conn
          ivan (:e (first (d/datoms db :avet :name "Ivan")))
          petr (:e (first (d/datoms db :avet :name "Petr")))]
      (testing "query"
        (is (= #{["Ivan" 30] ["Petr" 44]}
              (d/q '[:find ?n ?a :where [?e :name ?n] [?e :age ?a]] db))))

      (testing "pull and entity over refs"
        (is (= "Ivan" (get-in (d/pull db [{:friend [:name]}] petr) [:friend :name])))
        (is (= "Ivan" (:name (:friend (d/entity db petr))))))

      (testing "upsert redirects to the existing entity"
        (d/transact! conn [{:name "Ivan" :age 31}])
        (is (= [31] (mapv :v (d/datoms @conn :eavt ivan :age)))))

      (testing "retraction with history stays invisible, forward and reverse"
        (d/transact! conn [[:db/retract ivan :aka "Terrible"]])
        (let [db @conn]
          (is (= #{"I"} (set (map :v (d/datoms db :eavt ivan :aka)))))
          (is (= #{"I"} (set (map :v (filter #(= :aka (:a %))
                                       (d/rseek-datoms db :eavt ivan))))))))

      (testing "index-range"
        (is (= [31 44] (mapv :v (d/index-range @conn :age 0 100)))))

      (testing "snapshot isolation across stores"
        (let [snapshot @conn]
          (d/transact! conn [{:name "Oleg" :age 11}])
          (is (= [31 44] (mapv :v (d/index-range snapshot :age 0 100))))
          (is (= [11 31 44] (mapv :v (d/index-range @conn :age 0 100)))))))))

(deftest test-slatedb-store-deref-values
  ;; SlateDB caps keys at 64 KiB; deref attributes keep only a content hash
  ;; in the keys and put the value bytes into the SlateDB value of a
  ;; ("blob", <hash>) key — so values far beyond the key cap round-trip
  (let [conn  (d/conn-from-db
                (empty-slatedb-db {:doc/name  {:db/unique :db.unique/identity}
                                   :doc/model {:dbval/deref true}}))
        ;; ~300 KiB of EDN, far over the 64 KiB key cap
        model {:objects (mapv (fn [i] {:id i :content (apply str (repeat 100 "x"))})
                              (range 2500))}]
    (d/transact! conn [{:doc/name "big" :doc/model model}])
    (let [v (:doc/model (d/entity @conn [:doc/name "big"]))]
      (is (not (realized? v)))
      (is (= model @v)))
    (testing "re-assert is a no-op"
      (is (empty? (:tx-data (d/transact! conn [{:doc/name "big" :doc/model model}])))))
    (testing "blob keys do not leak into index scans"
      (is (= #{:doc/name :doc/model}
             (into #{} (map :a) (d/datoms @conn :eavt)))))
    (testing "update and retract"
      (d/transact! conn [{:doc/name "big" :doc/model (assoc model :v 2)}])
      (is (= (assoc model :v 2) @(:doc/model (d/entity @conn [:doc/name "big"]))))
      (d/transact! conn [[:db/retract [:doc/name "big"] :doc/model (assoc model :v 2)]])
      (is (nil? (:doc/model (d/entity @conn [:doc/name "big"])))))))

(deftest test-slatedb-store-transaction-isolation
  ;; a failing transaction must leave the store untouched: nothing is
  ;; written until the pending overlay commits atomically
  (let [conn (d/conn-from-db (empty-slatedb-db {:name {:db/unique :db.unique/identity}}))]
    (d/transact! conn [{:name "Ivan"}])
    (is (thrown? clojure.lang.ExceptionInfo
          (d/transact! conn [{:name "Oleg"}
                             [:db/add "x" :bad nil]])))
    (is (= ["Ivan"] (mapv :v (d/datoms @conn :aevt :name))))))
