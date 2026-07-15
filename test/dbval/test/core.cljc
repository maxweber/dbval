(ns dbval.test.core
  (:require
    [clojure.edn :as edn]
    [clojure.test :as t :refer [is are deftest testing]]
    [clojure.string :as str]
    [cognitect.transit :as transit]
    [dbval.core :as d]
    [dbval.impl.entity :as de]
    [dbval.db :as db]
    #?(:cljs [dbval.test.cljs])))

#?(:cljs
   (enable-console-print!))

;; Added special case for printing ex-data of ExceptionInfo
#?(:cljs
   (defmethod t/report [::t/default :error] [m]
     (t/inc-report-counter! :error)
     (println "\nERROR in" (t/testing-vars-str m))
     (when (seq (:testing-contexts (t/get-current-env)))
       (println (t/testing-contexts-str)))
     (when-let [message (:message m)] (println message))
     (println "expected:" (pr-str (:expected m)))
     (print "  actual: ")
     (let [actual (:actual m)]
       (cond
         (instance? ExceptionInfo actual)
         (println (.-stack actual) "\n" (pr-str (ex-data actual)))
         (instance? js/Error actual)
         (println (.-stack actual))
         :else
         (prn actual)))))

#?(:cljs (def test-summary (atom nil)))
#?(:cljs (defmethod t/report [::t/default :end-run-tests] [m]
           (reset! test-summary (dissoc m :type))))

(defn wrap-res [f]
  #?(:cljs (do (f) (clj->js @test-summary))
     :clj  (let [res (f)]
             (when (pos? (+ (:fail res) (:error res)))
               (System/exit 1)))))

;; utils
#?(:clj
   (defmethod t/assert-expr 'thrown-msg? [msg form]
     (let [[_ match & body] form]
       `(try ~@body
          (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch Throwable e#
            (let [m# (.getMessage e#)]
              (if (= ~match m#)
                (t/do-report {:type :pass, :message ~msg, :expected '~form, :actual e#})
                (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual e#})))
            e#)))))

(defn entity-map [db e]
  (when-let [entity (d/entity db e)]
    (->> (assoc (into {} entity) :db/id (:db/id entity))
      (clojure.walk/prewalk #(if (de/entity? %)
                               {:db/id (:db/id %)}
                               %)))))

(defn all-datoms [db]
  (into #{} (map (juxt :e :a :v)) (d/datoms db :eavt)))

#?(:clj
   (defn no-namespace-maps [t]
     (binding [*print-namespace-maps* false]
       (t)))
   :cljs
   (def no-namespace-maps {:before #(set! *print-namespace-maps* false)}))

(defn transit-write [o type]
  #?(:clj
     (with-open [os (java.io.ByteArrayOutputStream.)]
       (let [writer (transit/writer os type)]
         (transit/write writer o)
         (.toByteArray os)))
     :cljs
     (transit/write (transit/writer type) o)))

(defn transit-write-str [o]
  #?(:clj (String. ^bytes (transit-write o :json) "UTF-8")
     :cljs (transit-write o :json)))

(defn transit-read [s type]
  #?(:clj
     (with-open [is (java.io.ByteArrayInputStream. s)]
       (transit/read (transit/reader is type)))
     :cljs
     (transit/read (transit/reader type) s)))

(defn transit-read-str [s]
  #?(:clj  (transit-read (.getBytes ^String s "UTF-8") :json)
     :cljs (transit-read s :json)))

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
