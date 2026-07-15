(ns dbval.conn
  (:require
    [dbval.db :as db #?@(:cljs [:refer [DB FilteredDB]])])
  #?(:clj
     (:import
       [dbval.db DB FilteredDB])))

(defn- current-db
  "Derives the current database value from the underlying store. All
   transacted state lives in the store, so the connection only has to attach
   the latest transaction id to its in-memory db template (schema, caches,
   storage connection)."
  [{:keys [db]}]
  (let [max-tx (db/q-max-tx db)]
    (if (= max-tx (db/basis-tx db))
      db
      (db/with-max-tx db max-tx))))

;; A connection is not a state container: the store is the single source of
;; truth and `deref` derives the current database value from it. The `state`
;; atom only holds process-local context: the db template (schema, caches,
;; storage connection) and the listeners.
;;
;; Deliberately not an atom (`IAtom`): the previous implementation ran the
;; side-effecting, committing transaction inside `swap!`, so a concurrent
;; `:db` swap (e.g. `reset-schema!`) could fail the CAS and make `swap!`
;; re-run the transaction — committing it twice. Writes are serialized with
;; `locking` instead.
(deftype Conn [state]
  #?@(:clj
      [clojure.lang.IDeref
       (deref [this]
         ;; lock so that a deref never observes another thread's uncommitted
         ;; transaction through the shared storage connection
         (locking this
           (current-db @state)))]

      :cljs
      [IDeref
       (-deref [_]
         (current-db @state))]))

(defn conn? [conn]
  (instance? Conn conn))

(defn- make-conn [db]
  (->Conn (atom {:db        db
                 :listeners {}})))

(defn- conn-state [conn]
  (.-state ^Conn conn))

(defn ^:no-doc listeners [conn]
  (:listeners @(conn-state conn)))

(defn with
  ([db tx-data] (with db tx-data nil))
  ([db tx-data tx-meta]
   {:pre [(db/db? db)]}
   (let [q-max-tx (db/q-max-tx db)
         max-tx   (db/basis-tx db)]
     ;; Check that the storage hasn't been modified since this db snapshot was created.
     ;; q-max-tx is the latest tx in storage, max-tx is what this snapshot sees.
     ;; If q-max-tx > max-tx, storage has been modified.
     (when (pos? (compare q-max-tx max-tx))
       (throw (ex-info "underlying tuple store has already been modified"
                       {:max-tx max-tx
                        :q-max-tx q-max-tx}))))
   (if (instance? FilteredDB db)
     (throw (ex-info "Filtered DB cannot be modified" {:error :transaction/filtered}))
     (db/transact-tx-data (db/->TxReport db db [] {} tx-meta) tx-data))))

(defn ^DB db-with
  "Applies transaction to an immutable db value, returning new immutable db value. Same as `(:db-after (with db tx-data))`."
  [db tx-data]
  {:pre [(db/db? db)]}
  (:db-after (with db tx-data)))

(defn conn-from-db [db]
  {:pre [(db/db? db)]}
  (make-conn db))

(defn conn-from-datoms
  ([datoms]
   (conn-from-db (db/init-db datoms nil {})))
  ([datoms schema]
   (conn-from-db (db/init-db datoms schema {})))
  ([datoms schema opts]
   (conn-from-db (db/init-db datoms schema opts))))

(defn create-conn
  ([]
   (conn-from-db (db/empty-db nil {})))
  ([schema]
   (conn-from-db (db/empty-db schema {})))
  ([schema opts]
   (conn-from-db (db/empty-db schema opts))))

(defn ^:no-doc -transact!
  "Runs the transaction against the conn's store without notifying listeners.
   Used by `transact!` and the JS API, which notifies listeners itself.

   The db value is derived from the store under the write lock, so the
   snapshot check in `with` always sees the latest transaction id."
  [conn tx-data tx-meta]
  {:pre [(conn? conn)]}
  (locking conn
    (with @conn tx-data tx-meta)))

(defn transact!
  ([conn tx-data]
   (transact! conn tx-data nil))
  ([conn tx-data tx-meta]
   {:pre [(conn? conn)]}
   (locking conn
     (let [report (-transact! conn tx-data tx-meta)]
       (doseq [[_ callback] (listeners conn)]
         (callback report))
       report))))

(defn ^:no-doc reset-conn!
  "Low-level: points `conn` at a different db template. Does not notify
   listeners. Used by the JS API."
  [conn db]
  {:pre [(conn? conn) (db/db? db)]}
  (locking conn
    (swap! (conn-state conn) assoc :db db)
    db))

(defn reset-schema! [conn schema]
  {:pre [(conn? conn)]}
  (locking conn
    (let [db (db/with-schema @conn schema)]
      (swap! (conn-state conn) assoc :db db)
      db)))

(defn listen!
  ([conn callback]
   (listen! conn (rand) callback))
  ([conn key callback]
   {:pre [(conn? conn)]}
   (swap! (conn-state conn) update :listeners assoc key callback)
   key))

(defn unlisten! [conn key]
  {:pre [(conn? conn)]}
  (swap! (conn-state conn) update :listeners dissoc key))
