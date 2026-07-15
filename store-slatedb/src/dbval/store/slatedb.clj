(ns dbval.store.slatedb
  "SlateDB-backed tuple store: an embedded ordered key-value store built on
   object storage (S3, GCS, or the local filesystem during development).

   dbval only needs the key portion, so every key is stored with an empty
   value. `-commit!` writes the batch through a SlateDB WriteBatch, which is
   atomic; scans use SlateDB's native ascending/descending iteration.

   This namespace lives in the store-slatedb module because
   io.slatedb/slatedb-uniffi is a heavy native dependency and this namespace
   cannot load without it."
  (:require
    [clojure.string :as str]
    [dbval.store :as store])
  (:import
    [java.util.concurrent CompletableFuture]
    [io.slatedb.uniffi Db DbBuilder ObjectStore DbIterator KeyValue WriteBatch
                       KeyRange ScanOptions IterationOrder DurabilityLevel]))

(set! *warn-on-reflection* true)

(def ^:private ^bytes EMPTY_VALUE
  "Empty byte array used as value for key-only puts in SlateDB."
  (byte-array 0))

(defonce ^:private native-lib-loaded
  ;; The slatedb-uniffi jar bundles the native library in JNA resource layout
  ;; (e.g. linux-x86-64/libslatedb_uniffi.so), but its generated loader only
  ;; calls System/loadLibrary. Extract the bundled library via JNA and point
  ;; the uniffi loader at it before the first native call.
  (delay
    (when-not (System/getProperty "uniffi.component.slatedb.libraryOverride")
      (let [lib (com.sun.jna.Native/extractFromResourcePath "slatedb_uniffi")]
        (System/setProperty "uniffi.component.slatedb.libraryOverride"
                            (.getAbsolutePath lib))))
    true))

(defn- await-future
  "Blocks on a CompletableFuture and returns its value.
   The uniffi bindings expose all SlateDB operations as async futures."
  [^CompletableFuture fut]
  (.join fut))

(defn- scan-options
  "ScanOptions with library defaults and the given iteration order."
  ^ScanOptions [reverse?]
  (ScanOptions. DurabilityLevel/MEMORY false 1 false 1
                (if reverse? IterationOrder/DESCENDING IterationOrder/ASCENDING)
                nil))

(defn- scan-iterator
  ^java.util.Iterator [^Db db ^bytes begin ^bytes end reverse?]
  (let [^DbIterator iter (await-future
                          (.scanWithOptions db
                                            (KeyRange. begin true end false)
                                            (scan-options reverse?)))
        next-val (atom nil)
        advanced (atom false)
        closed   (atom false)
        close!   (fn []
                   (when-not @closed
                     (reset! closed true)
                     (try (.close iter) (catch Throwable _))))
        advance! (fn []
                   (when-not @closed
                     (if-some [^KeyValue kv (await-future (.next iter))]
                       (do (reset! next-val (.key kv)) true)
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

(deftype SlateDbStore [^Db db db-file]
  store/ITupleStore
  (-scan [_ begin end reverse?]
    (reify java.lang.Iterable
      (iterator [_]
        (scan-iterator db begin end (boolean reverse?)))))

  (-commit! [this keys]
    (when (seq keys)
      (locking this
        (let [batch (WriteBatch.)]
          (try
            (doseq [^bytes k keys]
              (.put batch k EMPTY_VALUE))
            (await-future (.write db batch))
            (finally
              ;; on success the batch contents were consumed by the write;
              ;; on error the batch is simply discarded
              (try (.close batch) (catch Throwable _))))))))

  (-close! [_]
    (.close db)))

(defn store
  "Opens a SlateDB-backed tuple store.

   Options:

   :db-file          <string>  Path of this store inside the object store.
                     Defaults to a fresh temporary path.
   :object-store-url <string>  URL of the object store backing this database
                     (e.g. \"file:///\", \"memory:///\" or an s3:// URL).
                     Defaults to the local filesystem."
  ^dbval.store.slatedb.SlateDbStore [{:keys [db-file object-store-url]}]
  @native-lib-loaded
  (let [tmp-dir (System/getProperty "java.io.tmpdir")
        db-file (or db-file
                    (str tmp-dir java.io.File/separator "dbval-" (random-uuid)))
        url     (or object-store-url "file:///")
        ;; the db path is relative to the object store root
        path    (str/replace db-file #"^/+" "")
        db      (with-open [obj-store (ObjectStore/resolve url)
                            builder   (DbBuilder. path obj-store)]
                  (await-future (.build builder)))]
    (SlateDbStore. db db-file)))
