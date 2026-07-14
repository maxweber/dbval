(ns dbval.test.db
  (:require
    [clojure.data]
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db :refer [defrecord-updatable]]
    [dbval.test.core]))

;;
;; verify that defrecord-updatable works with compiler/core macro configuration
;; define dummy class which redefines hash, could produce either
;; compiler or runtime error
;;
(defrecord-updatable HashBeef [x]
  #?@(:cljs [IHash                (-hash  [hb] 0xBEEF)]
      :clj  [clojure.lang.IHashEq (hasheq [hb] 0xBEEF)]))

(deftest test-defrecord-updatable
  (is (= 0xBEEF (-> (map->HashBeef {:x :ignored}) hash))))


;; whitebox test to confirm that hash cache caches
(deftest test-db-hash-cache
  (let [db (d/empty-db)]
    (is (= 0         @(.-hash db)))
    (let [h (hash db)]
      (is (= h @(.-hash db))))))

(defn- now []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(deftest test-uuid
  (let [now-ms (loop []
                 (let [ts (now)]
                   (if (> (mod ts 1000) 900) ;; sleeping over end of a second
                     (recur)
                     ts)))
        now    (int (/ now-ms 1000))]
    (is (= (* 1000 now) (d/squuid-time-millis (d/squuid))))
    (is (not= (d/squuid) (d/squuid)))
    (is (= (subs (str (d/squuid)) 0 8)
          (subs (str (d/squuid)) 0 8)))))

(deftest test-diff
  ;; clojure.data/diff is deliberately unsupported: dbval databases may not
  ;; fit into memory, and a diff would have to realize both sides entirely.
  ;; The DB record still extends clojure.data/Diff, but only to throw —
  ;; otherwise diff would fall back to clojure.data's default map
  ;; implementation and diff the record's fields.
  (let [tx1 (d/with (d/empty-db) [{:db/id "e1" :a 1 :b 2}])
        db1 (:db-after tx1)
        e1 (get (:tempids tx1) "e1")
        db2 (d/db-with db1 [[:db/retract e1 :b 2]
                            [:db/add e1 :c 3]])]
    (is (thrown-msg? "clojure.data/diff is not supported on dbval databases, since it would realize both databases entirely in memory"
          (clojure.data/diff db1 db2)))))
