(ns dbval.test.issues
  (:require
    [clojure.data]
    [dbval.core :as ds]
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.test.core]))

(deftest ^{:doc "CLJS `apply` + `vector` will hold onto mutable array of arguments directly"}
  issue-262
  (let [db (ds/db-with (ds/empty-db)
             [{:attr "A"} {:attr "B"}])]
    (is (= (ds/q '[:find ?a ?b
                   :where [_ :attr ?a] 
                   [(vector ?a) ?b]]
             db)
          #{["A" ["A"]] ["B" ["B"]]}))))

(deftest ^{:doc "Can't pprint filtered db"}
     issue-330
     (let [base     (-> (ds/empty-db {:aka {:db/cardinality :db.cardinality/many}})
                      (ds/db-with [{:db/id -1
                                    :name  "Maksim"
                                    :age   45
                                    :aka   ["Max Otto von Stierlitz", "Jack Ryan"]}]))
           filtered (ds/filter base (constantly true))]
       (t/is (= (with-out-str (clojure.pprint/pprint base))
               (with-out-str (clojure.pprint/pprint filtered))))))

(deftest ^{:doc "Can't diff databases with different types of the same attribute"}
  issue-369
  ;; The original issue is moot: clojure.data/diff is deliberately
  ;; unsupported on dbval databases (see dbval.test.db/test-diff).
  (let [eid #uuid "22222222-2222-2222-2222-222222222222"
        db1 (-> (ds/empty-db)
              (ds/db-with [[:db/add eid :attr :aa]]))
        db2 (-> (ds/empty-db)
              (ds/db-with [[:db/add eid :attr "aa"]]))]
    (t/is (thrown-msg? "clojure.data/diff is not supported on dbval databases, since it would realize both databases entirely in memory"
            (clojure.data/diff db1 db2)))))

(deftest ^{:doc "Expose a schema as a part of the public API."}
  issue-381
  (let [schema {:aka {:db/cardinality :db.cardinality/many}}
        db     (ds/empty-db schema)]
    (t/is (= schema (ds/schema db)))))
