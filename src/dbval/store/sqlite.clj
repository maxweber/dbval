(ns dbval.store.sqlite
  "SQLite-backed tuple store: one table holding the sorted keys.

       create table dbval (k blob not null, primary key(k)) WITHOUT ROWID;

   The JDBC connection runs with autocommit on, so every scan reads the
   latest committed state (no lingering WAL read transaction pinning an old
   snapshot); `-commit!` wraps its batch insert in a single transaction."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [dbval.store :as store]))

(set! *warn-on-reflection* true)

(defn- sqlite-jdbc-url
  "Builds a full SQLite JDBC URL with optional pragmas."
  [db-file {:keys [busy-timeout-ms foreign-keys? wal? read-only? mmap-size sync]
            :or   {busy-timeout-ms 5000
                   foreign-keys?   true
                   wal?            true
                   read-only?      false
                   sync            "NORMAL"}}]
  (let [params (cond-> {"busy_timeout" busy-timeout-ms
                        "foreign_keys" (if foreign-keys? "on" "off")
                        "synchronous"  (str sync)}
                 wal?       (assoc "journal_mode" "WAL")
                 read-only? (assoc "mode" "ro")
                 mmap-size  (assoc "mmap_size" mmap-size))
        qs     (->> params
                    (map (fn [[k v]] (str k "=" v)))
                    (str/join "&"))]
    (str "jdbc:sqlite:" db-file (when (seq qs) (str "?" qs)))))

(defn- load-driver! []
  (try
    (java.lang.Class/forName "org.sqlite.JDBC")
    (catch ClassNotFoundException e
      (throw (ex-info
               (str "SQLite JDBC driver not found on the classpath. "
                    "dbval ships no storage driver: add org.xerial/sqlite-jdbc "
                    "to your dependencies to use the default SQLite store, "
                    "or pass an explicit :store to empty-db "
                    "(e.g. dbval.store.memory).")
               {:error :store/missing-driver}
               e)))))

(defn- ^java.sql.Connection get-connection
  [db-file opts]
  (load-driver!)
  (let [^String url (sqlite-jdbc-url db-file opts)]
    (java.sql.DriverManager/getConnection url)))

(defn- create-table! [^java.sql.Connection conn]
  (with-open [stmt (.createStatement conn)]
    (.execute ^java.sql.Statement stmt
      "create table if not exists dbval (k blob not null, primary key(k)) WITHOUT ROWID;")))

(defn- scan-iterator
  ^java.util.Iterator [^java.sql.Connection conn ^bytes begin ^bytes end reverse?]
  (let [^java.sql.PreparedStatement stmt
        (.prepareStatement conn
                           (str "select k from dbval where k >= ? and k < ?"
                                (when reverse?
                                  " order by k desc"))
                           java.sql.ResultSet/TYPE_FORWARD_ONLY
                           java.sql.ResultSet/CONCUR_READ_ONLY)
        _ (.setBytes stmt 1 begin)
        _ (.setBytes stmt 2 end)
        _ (.setFetchSize ^java.sql.Statement stmt 1000) ; hint (SQLite may ignore)
        ^java.sql.ResultSet rs (.executeQuery stmt)

        next-val (atom nil)
        advanced (atom false)
        closed   (atom false)
        close!   (fn []
                   (when-not @closed
                     (reset! closed true)
                     (try (.close rs)   (catch Throwable _))
                     (try (.close stmt) (catch Throwable _))))
        advance! (fn []
                   (when-not @closed
                     (if (.next rs)
                       (do (reset! next-val (.getBytes rs "k")) true)
                       (do (reset! next-val nil) (close!) false))))]
    (reify java.util.Iterator
      (hasNext [this]
        (or @advanced
            (reset! advanced
                    (boolean (advance!)))))
      (next [_]
        (let [ok (or @advanced (advance!))]
          (when-not ok
            (throw (java.util.NoSuchElementException.)))
          (let [v @next-val]
            (reset! advanced false)
            (reset! next-val nil)
            v)))
      (remove [_]
        (throw (UnsupportedOperationException. "remove not supported"))))))

(deftype SqliteStore [^java.sql.Connection conn db-file]
  store/ITupleStore
  (-scan [_ begin end reverse?]
    (reify java.lang.Iterable
      (iterator [_]
        (scan-iterator conn begin end (boolean reverse?)))))

  (-commit! [this keys]
    (when (seq keys)
      (locking this
        (.setAutoCommit conn false)
        (try
          (with-open [stmt (.prepareStatement conn "INSERT OR IGNORE INTO dbval (k) VALUES (?)")]
            (doseq [^bytes k keys]
              (.setBytes stmt 1 k)
              (.addBatch stmt))
            (.executeBatch stmt))
          (.commit conn)
          (catch Throwable t
            (try (.rollback conn) (catch Throwable _))
            (throw t))
          (finally
            (.setAutoCommit conn true))))))

  (-close! [_]
    (.close conn)))

(defn store
  "Opens (creating if necessary) a SQLite-backed tuple store.

   Options:

   :db-file <string>  Path to the SQLite file. Defaults to a fresh
                      temporary file.
   :opts    <map>     SQLite pragmas: :busy-timeout-ms, :foreign-keys?,
                      :wal?, :read-only?, :mmap-size, :sync."
  ^dbval.store.sqlite.SqliteStore [{:keys [db-file opts]}]
  (let [db-file (or db-file
                    (.getCanonicalPath
                     (java.io.File/createTempFile (str (random-uuid))
                                                  ".db")))
        _       (io/make-parents db-file)
        conn    (get-connection db-file opts)]
    (create-table! conn)
    (SqliteStore. conn db-file)))
