(ns dbval.test.core
  (:require
    [clojure.edn :as edn]
    [clojure.test :as t :refer [is are deftest testing]]
    [clojure.string :as str]
    [cognitect.transit :as transit]
    [dbval.core :as d]
    [dbval.impl.entity :as de]
    [dbval.db :as db]
    ))


;; Added special case for printing ex-data of ExceptionInfo


(defn wrap-res [f]
  (let [res (f)]
             (when (pos? (+ (:fail res) (:error res)))
               (System/exit 1))))

;; utils
(defmethod t/assert-expr 'thrown-msg? [msg form]
     (let [[_ match & body] form]
       `(try ~@body
          (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch Throwable e#
            (let [m# (.getMessage e#)]
              (if (= ~match m#)
                (t/do-report {:type :pass, :message ~msg, :expected '~form, :actual e#})
                (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual e#})))
            e#))))

(defn entity-map [db e]
  (when-let [entity (d/entity db e)]
    (->> (assoc (into {} entity) :db/id (:db/id entity))
      (clojure.walk/prewalk #(if (de/entity? %)
                               {:db/id (:db/id %)}
                               %)))))

(defn all-datoms [db]
  (into #{} (map (juxt :e :a :v)) (d/datoms db :eavt)))

(defn no-namespace-maps [t]
     (binding [*print-namespace-maps* false]
       (t)))

(defn transit-write [o type]
  (with-open [os (java.io.ByteArrayOutputStream.)]
       (let [writer (transit/writer os type)]
         (transit/write writer o)
         (.toByteArray os))))

(defn transit-write-str [o]
  (String. ^bytes (transit-write o :json) "UTF-8"))

(defn transit-read [s type]
  (with-open [is (java.io.ByteArrayInputStream. s)]
       (transit/read (transit/reader is type))))

(defn transit-read-str [s]
  (transit-read (.getBytes ^String s "UTF-8") :json))

;; Core tests

(deftest test-protocols
  (let [schema {:aka {:db/cardinality :db.cardinality/many}
                :name {:db/unique :db.unique/identity}}
        tx (d/with (d/empty-db schema)
             [{:db/id "ivan" :name "Ivan" :aka ["IV" "Terrible"]}
              {:db/id "petr" :name "Petr" :age 37 :huh? false}])
        db (:db-after tx)
        ivan-id (get (:tempids tx) "ivan")
        petr-id (get (:tempids tx) "petr")]
    ;; db values are opaque handles (deftype), not maps
    (is (not (map? db)))
    (is (= (set (d/datoms db :eavt))
          #{(d/datom ivan-id :aka "IV")
            (d/datom ivan-id :aka "Terrible")
            (d/datom ivan-id :name "Ivan")
            (d/datom petr-id :age 37)
            (d/datom petr-id :name "Petr")
            (d/datom petr-id :huh? false)}))))
