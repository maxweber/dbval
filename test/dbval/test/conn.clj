(ns dbval.test.conn
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(def schema
  {:aka {:db/cardinality :db.cardinality/many}})

;; Use a fixed UUID for deterministic testing
(def test-eid #uuid "11111111-1111-1111-1111-111111111111")

(def datoms
  #{(d/datom test-eid :age  17)
    (d/datom test-eid :name "Ivan")})

(deftest test-ways-to-create-conn
  (let [conn (d/create-conn)]
    (is (= #{} (set (d/datoms @conn :eavt))))
    (is (= nil (d/schema @conn))))

  (let [conn (d/create-conn schema)]
    (is (= #{} (set (d/datoms @conn :eavt))))
    (is (= schema (d/schema @conn))))

  (let [conn (d/conn-from-datoms datoms)]
    (is (= datoms (set (d/datoms @conn :eavt))))
    (is (= nil (d/schema @conn))))

  (let [conn (d/conn-from-datoms datoms schema)]
    (is (= datoms (set (d/datoms @conn :eavt))))
    (is (= schema (d/schema @conn))))

  (let [conn (d/conn-from-db (d/init-db datoms))]
    (is (= datoms (set (d/datoms @conn :eavt))))
    (is (= nil (d/schema @conn))))

  (let [conn (d/conn-from-db (d/init-db datoms schema))]
    (is (= datoms (set (d/datoms @conn :eavt))))
    (is (= schema (d/schema @conn)))))

(deftest test-conn-is-not-an-atom
  ;; the store is the single source of truth; the conn is a handle, not a
  ;; state container, so it deliberately does not support swap!/reset!
  (let [conn (d/create-conn)]
    (is (d/conn? conn))
    (is (not (instance? clojure.lang.IAtom conn)))
    (is (not (d/conn? (atom nil))))))

(deftest test-deref-derives-value-from-store
  ;; deref queries the store for the latest transaction id, so it also sees
  ;; transactions that did not go through this conn's transact!
  (let [conn (d/create-conn)
        db1  @conn]
    (d/db-with db1 [{:name "Ivan"}])
    (is (= ["Ivan"] (mapv :v (d/datoms @conn :aevt :name))))))

(deftest test-with-rejects-stale-snapshot
  ;; a db value whose store has been modified since the snapshot was taken
  ;; cannot be transacted against
  (let [conn (d/create-conn)
        db1  @conn]
    (d/db-with db1 [{:name "Ivan"}])
    (is (thrown-msg? "underlying tuple store has already been modified"
          (d/db-with db1 [{:name "Oleg"}])))))

(deftest test-deref-sees-other-connections
  ;; two connections to the same SQLite file: reads run with autocommit, so
  ;; a deref always sees the latest committed transaction instead of a
  ;; pinned WAL read snapshot
  (let [db-file (str (System/getProperty "java.io.tmpdir")
                  "/dbval-test-" (random-uuid) ".db")
        conn1   (d/create-conn nil {:db-file db-file})
        conn2   (d/create-conn nil {:db-file db-file})]
    (d/transact! conn1 [{:name "Ivan"}])
    (is (= ["Ivan"] (mapv :v (d/datoms @conn2 :aevt :name))))))

(deftest test-transact!-not-repeated-by-concurrent-conn-update
     ;; regression: `-transact!` used to run the (side-effecting, committing)
     ;; transaction inside `swap!`; a concurrent update of the conn state
     ;; (e.g. `reset-schema!`) could fail the CAS and make `swap!` re-run the
     ;; transaction — committing it twice under two different tx ids
     (let [conn        (d/create-conn)
           transacting (future
                         (d/transact! conn
                           [[:db.fn/call (fn [_db]
                                           (Thread/sleep 100)
                                           [{:x "hello"}])]]))]
       (Thread/sleep 30)
       (d/reset-schema! conn {:y {:db/index true}})
       @transacting
       (is (= ["hello"] (mapv :v (d/datoms @conn :aevt :x))))
       (is (= {:y {:db/index true}} (d/schema @conn)))))
