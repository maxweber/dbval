(ns dbval.test.listen
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(deftest test-listen!
  (let [conn    (d/create-conn)
        reports (atom [])]
    ;; First transaction - setup, not listened
    (let [tx1 (d/transact! conn [[:db/add "alex" :name "Alex"]
                                  [:db/add "boris" :name "Boris"]])
          alex-id (get (:tempids tx1) "alex")
          boris-id (get (:tempids tx1) "boris")]

      (d/listen! conn :test #(swap! reports conj %))

      ;; Second transaction - listened
      (let [tx2 (d/transact! conn [[:db/add "dima" :name "Dima"]
                                    [:db/add "dima" :age 19]
                                    [:db/add "evgeny" :name "Evgeny"]] {:some-metadata 1})
            dima-id (get (:tempids tx2) "dima")
            evgeny-id (get (:tempids tx2) "evgeny")
            tx2-id (:tx tx2)]

        ;; Third transaction - listened
        (let [tx3 (d/transact! conn [[:db/add "fedor" :name "Fedor"]
                                      [:db/add alex-id :name "Alex2"]       ;; should update
                                      [:db/retract boris-id :name "Not Boris"] ;; should be skipped (wrong value)
                                      [:db/retract evgeny-id :name "Evgeny"]])
              fedor-id (get (:tempids tx3) "fedor")
              tx3-id (:tx tx3)]

          (d/unlisten! conn :test)
          (d/transact! conn [[:db/add "geogry" :name "Geogry"]])

          ;; Check first listened report (tx2)
          (is (= (set (map (fn [d] [(:e d) (:a d) (:v d) (:added d)]) (:tx-data (first @reports))))
                #{[dima-id :name "Dima" true]
                  [dima-id :age 19 true]
                  [evgeny-id :name "Evgeny" true]}))
          (is (= (-> (first @reports) :tx-data first :tx) tx2-id))
          (is (= (:tx-meta (first @reports))
                {:some-metadata 1}))

          ;; Check second listened report (tx3)
          (is (= (set (map (fn [d] [(:e d) (:a d) (:v d) (:added d)]) (:tx-data (second @reports))))
                #{[fedor-id :name "Fedor" true]
                  [alex-id :name "Alex" false]    ;; update -> retract
                  [alex-id :name "Alex2" true]    ;;         + add
                  [evgeny-id :name "Evgeny" false]}))
          (is (= (-> (second @reports) :tx-data first :tx) tx3-id))
          (is (= (:tx-meta (second @reports))
                nil)))))))
