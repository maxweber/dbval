(ns dbval.test.upsert
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))


(deftest test-upsert
  (let [schema  {:name  {:db/unique      :db.unique/identity}
                 :email {:db/unique      :db.unique/identity}
                 :slugs {:db/unique      :db.unique/identity
                         :db/cardinality :db.cardinality/many}
                 :ref   {:db/unique      :db.unique/identity
                         :db/type        :db.type/ref}}
        db*     (fn []
                  (let [tx (d/with (d/empty-db schema)
                            [{:db/id "ivan" :name "Ivan" :email "@1"}
                             {:db/id "petr" :name "Petr" :email "@2" :ref "dima"}
                             {:db/id "dima" :name "Dima" :email "@3" :ref "olga"}
                             {:db/id "olga" :name "Olga" :email "@4" :ref "ivan"}])]
                    {:db (:db-after tx)
                     :ivan (get (:tempids tx) "ivan")
                     :petr (get (:tempids tx) "petr")
                     :dima (get (:tempids tx) "dima")
                     :olga (get (:tempids tx) "olga")}))
        pull    (fn [tx e]
                  (d/pull (:db-after tx) ['* {[:ref :xform #(:db/id %)] [:db/id]}] e))
        tempids (fn [tx]
                  (dissoc (:tempids tx) :db/current-tx))]
    (testing "upsert, no tempid"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:name "Ivan" :age 35}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 35}
              (pull tx [:name "Ivan"])))
        (is (= {}
              (tempids tx)))))

    (testing "upsert by 2 attrs, no tempid"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:name "Ivan" :email "@1" :age 35}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 35}
              (pull tx [:name "Ivan"])))
        (is (= {}
              (tempids tx)))))

    (testing "upsert with tempid"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:db/id -1 :name "Ivan" :age 35}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 35}
              (pull tx [:name "Ivan"])))
        (is (= ivan (get (tempids tx) -1)))))

    (testing "upsert with string tempid"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:db/id "user" :name "Ivan" :age 35}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 35}
               (pull tx [:name "Ivan"])))
        (is (= ivan (get (tempids tx) "user")))))

    (testing "upsert by 2 attrs with tempid"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:db/id -1 :name "Ivan" :email "@1" :age 35}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 35}
              (pull tx [:name "Ivan"])))
        (is (= ivan (get (tempids tx) -1)))))

    (testing "upsert to two entities, resolve to same tempid"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:db/id -1 :name "Ivan" :age 35}
                           {:db/id -1 :name "Ivan" :age 36}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 36}
              (pull tx [:name "Ivan"])))
        (is (= ivan (get (tempids tx) -1)))))

    (testing "upsert to two entities, two tempids"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:db/id -1 :name "Ivan" :age 35}
                           {:db/id -2 :name "Ivan" :age 36}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 36}
              (pull tx [:name "Ivan"])))
        (is (= ivan (get (tempids tx) -1)))
        (is (= ivan (get (tempids tx) -2)))))

    (testing "upsert with existing id (lookup ref)"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:db/id [:name "Ivan"] :name "Ivan" :age 35}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 35}
              (pull tx [:name "Ivan"])))
        (is (= (tempids tx)
              {}))))

    (testing "upsert by 2 attrs with existing id as lookup ref"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:db/id [:name "Ivan"] :name "Ivan" :email "@1" :age 35}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :age 35}
              (pull tx [:name "Ivan"])))
        (is (= (tempids tx)
              {}))))

    (testing "upsert conflicts with different existing lookup ref"
      (let [{:keys [db]} (db*)]
        (is (thrown-with-msg? Throwable #"Conflicting upsert: \[:name \"Ivan\"\] resolves to .*, but entity already has :db/id"
                              (d/with db [{:db/id [:name "Petr"] :name "Ivan" :age 36}])))))

    (testing "upsert by non-existing value resolves as update"
      (let [{:keys [db ivan]} (db*)
            tx (d/with db [{:name "Ivan" :email "@5" :age 35}])]
        (is (= {:db/id ivan :name "Ivan" :email "@5" :age 35}
              (pull tx [:name "Ivan"])))
        (is (= {}
              (tempids tx)))))

    (testing "upsert by 2 conflicting fields"
      (let [{:keys [db]} (db*)]
        (is (thrown-with-msg? Throwable #"Conflicting upserts: \[:name \"Ivan\"\] resolves to .*, but \[:email \"@2\"\] resolves to"
                              (d/with db [{:name "Ivan" :email "@2" :age 35}])))))

    (testing "upsert over intermediate db"
      (let [{:keys [db]} (db*)
            tx (d/with db [{:name "Igor" :age 35}
                           {:name "Igor" :age 36}])]
        (is (= 36 (:age (pull tx [:name "Igor"]))))
        (is (= {}
              (tempids tx)))))

    (testing "upsert over intermediate db, tempids"
      (let [{:keys [db]} (db*)
            tx (d/with db [{:db/id -1 :name "Igor" :age 35}
                           {:db/id -1 :name "Igor" :age 36}])]
        (is (= 36 (:age (pull tx [:name "Igor"]))))
        (is (uuid? (get (tempids tx) -1)))))

    (testing "upsert over intermediate db, different tempids"
      (let [{:keys [db]} (db*)
            tx (d/with db [{:db/id -1 :name "Igor" :age 35}
                           {:db/id -2 :name "Igor" :age 36}])]
        (is (= 36 (:age (pull tx [:name "Igor"]))))
        (let [igor-id (get (tempids tx) -1)]
          (is (uuid? igor-id))
          (is (= igor-id (get (tempids tx) -2))))))

    (testing "upsert and :current-tx conflict"
      (let [{:keys [db]} (db*)]
        (is (thrown-with-msg? Throwable #"Conflicting upsert: \[:name \"Ivan\"\] resolves to .*, but entity already has :db/id"
                              (d/with db [{:db/id :db/current-tx :name "Ivan" :age 35}])))))

    (testing "upsert of unique, cardinality-many values"
      (let [{:keys [db ivan petr]} (db*)
            tx  (d/with db [{:name "Ivan" :slugs "ivan1"}
                            {:name "Petr" :slugs "petr1"}])
            tx2 (d/with (:db-after tx) [{:name "Ivan" :slugs ["ivan1" "ivan2"]}])]
        (is (= {:db/id ivan :name "Ivan" :email "@1" :slugs ["ivan1"]}
              (pull tx [:name "Ivan"])))
        (is (= {:db/id ivan :name "Ivan" :email "@1" :slugs ["ivan1" "ivan2"]}
              (pull tx2 [:name "Ivan"])))
        (is (thrown-with-msg? Throwable #"Conflicting upserts:"
                              (d/with (:db-after tx2) [{:slugs ["ivan1" "petr1"]}])))))

    (testing "upsert by ref"
      ;; Each test needs a fresh db to avoid "underlying tuple store modified" errors
      (let [{:keys [db petr dima]} (db*)
            tx (d/with db [{:ref dima :age 36}])]
        (is (= {:db/id petr :name "Petr" :email "@2" :ref dima :age 36}
               (pull tx [:name "Petr"]))))
      (let [{:keys [db dima olga]} (db*)
            tx (d/with db [{:ref olga :age 37}])]
        (is (= {:db/id dima :name "Dima" :email "@3" :ref olga :age 37}
               (pull tx [:name "Dima"]))))
      (let [{:keys [db olga ivan]} (db*)
            tx (d/with db [{:ref ivan :age 38}])]
        (is (= {:db/id olga :name "Olga" :email "@4" :ref ivan :age 38}
              (pull tx [:name "Olga"])))))

    (testing "upsert by lookup ref"
      ;; Each test needs a fresh db to avoid "underlying tuple store modified" errors
      (let [{:keys [db petr dima]} (db*)
            tx (d/with db [{:ref [:name "Dima"] :age 36}])]
        (is (= {:db/id petr :name "Petr" :email "@2" :ref dima :age 36}
               (pull tx [:name "Petr"]))))
      (let [{:keys [db dima olga]} (db*)
            tx (d/with db [{:ref [:name "Olga"] :age 37}])]
        (is (= {:db/id dima :name "Dima" :email "@3" :ref olga :age 37}
               (pull tx [:name "Dima"]))))
      (let [{:keys [db olga ivan]} (db*)
            tx (d/with db [{:ref [:name "Ivan"] :age 38}])]
        (is (= {:db/id olga :name "Olga" :email "@4" :ref ivan :age 38}
              (pull tx [:name "Olga"])))))

    ;; issue-464
    (testing "not upsert by ref"
      (let [{:keys [db]} (db*)
            tx (d/with db [{:db/id -1 :name "Igor"}
                           {:db/id -2 :name "Anna" :ref -1}])
            igor (get (tempids tx) -1)
            anna (get (tempids tx) -2)]
        (is (= {:db/id igor :name "Igor"} (pull tx [:name "Igor"])))
        (is (= {:db/id anna :name "Anna" :ref igor} (pull tx [:name "Anna"]))))

      (let [{:keys [db]} (db*)
            tx (d/with db [{:db/id "A" :name "Igor"}
                           {:db/id "B" :name "Anna" :ref "A"}])
            igor (get (tempids tx) "A")
            anna (get (tempids tx) "B")]
        (is (= {:db/id igor :name "Igor"} (pull tx [:name "Igor"])))
        (is (= {:db/id anna :name "Anna" :ref igor} (pull tx [:name "Anna"])))))))

(deftest test-redefining-ids
  ;; Test that tempids properly resolve to existing entities via upsert
  (let [tx1 (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
              [{:db/id "ivan" :name "Ivan"}])
        ivan (get (:tempids tx1) "ivan")
        db (:db-after tx1)]
    (let [tx (d/with db [{:db/id -1 :age 35}
                         {:db/id -1 :name "Ivan" :age 36}])
          ivan2 (get (:tempids tx) -1)]
      ;; The tempid -1 should resolve to Ivan's UUID via upsert
      (is (= ivan ivan2))
      (is (= #{[:age 36] [:name "Ivan"]}
            (set (map (fn [d] [(:a d) (:v d)]) (d/datoms (:db-after tx) :eavt ivan)))))))

  ;; Test conflicting upserts
  (let [tx1 (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
              [{:db/id "ivan" :name "Ivan"}
               {:db/id "oleg" :name "Oleg"}])
        db (:db-after tx1)]
    (is (thrown-with-msg? Throwable #"Conflicting upsert:.*resolves to.*but entity already has"
          (d/with db [{:db/id -1 :name "Ivan" :age 35}
                      {:db/id -1 :name "Oleg" :age 36}])))))

;; issue-285
(deftest test-retries-order
  (let [tx (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
             [[:db/add -1 :age 42]
              [:db/add -2 :likes "Pizza"]
              [:db/add -1 :name "Bob"]
              [:db/add -2 :name "Bob"]])
        bob (get (:tempids tx) -1)]
    (is (= {:db/id bob, :name "Bob", :likes "Pizza", :age 42}
          (tdc/entity-map (:db-after tx) [:name "Bob"]))))

  (let [tx (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
             [[:db/add -1 :age 42]
              [:db/add -2 :likes "Pizza"]
              [:db/add -2 :name "Bob"]
              [:db/add -1 :name "Bob"]])
        bob (get (:tempids tx) -2)]
    (is (= {:db/id bob, :name "Bob", :likes "Pizza", :age 42}
          (tdc/entity-map (:db-after tx) [:name "Bob"])))))

;; issue-403
(deftest test-upsert-string-tempid-ref
  (let [schema {:name {:db/unique :db.unique/identity}
                :ref {:db/valueType :db.type/ref}
                :age {:db/index true}}
        db*   (fn []
                (let [tx (d/with (d/empty-db schema) [{:db/id "alice" :name "Alice"}])]
                  {:db (:db-after tx)
                   :alice (get (:tempids tx) "alice")}))]
    ;; Test with string tempid upsert
    (let [{:keys [db alice]} (db*)
          tx (d/with db [{:db/id "user", :name "Alice"}
                         {:age 36, :ref "user"}])
          new-entity (some #(when (= 36 (get % :age)) %)
                       (map #(tdc/entity-map (:db-after tx) (:e %))
                         (d/datoms (:db-after tx) :avet :age 36)))]
      ;; "user" should upsert to Alice
      (is (= alice (get (:tempids tx) "user")))
      ;; entity-map wraps refs in {:db/id uuid}, so extract :db/id
      (is (= alice (:db/id (:ref new-entity)))))

    ;; Test with :db/add and string tempid
    (let [{:keys [db alice]} (db*)
          tx (d/with db [[:db/add "user" :name "Alice"]
                         {:age 36, :ref "user"}])]
      (is (= alice (get (:tempids tx) "user"))))

    ;; Test with negative tempid
    (let [{:keys [db alice]} (db*)
          tx (d/with db [{:db/id -1, :name "Alice"}
                         {:age 36, :ref -1}])]
      (is (= alice (get (:tempids tx) -1))))

    ;; Test with :db/add and negative tempid
    (let [{:keys [db alice]} (db*)
          tx (d/with db [[:db/add -1, :name "Alice"]
                         {:age 36, :ref -1}])]
      (is (= alice (get (:tempids tx) -1))))))

;; issue-472
(deftest test-two-tempids-two-retries
  (let [schema   {:name {:db/unique :db.unique/identity}
                  :ref {:db/valueType :db.type/ref}}
        tx0      (d/with (d/empty-db schema)
                   [{:db/id "alice" :name "Alice"}
                    {:db/id "bob" :name "Bob"}])
        db       (:db-after tx0)
        alice    (get (:tempids tx0) "alice")
        bob      (get (:tempids tx0) "bob")
        tx       (d/with db
                   [{:db/id "e3", :ref "A"}
                    {:db/id "e4", :ref "B"}
                    {:db/id "A", :name "Alice"}
                    {:db/id "B", :name "Bob"}])
        e3       (get (:tempids tx) "e3")
        e4       (get (:tempids tx) "e4")]
    ;; "A" should upsert to Alice, "B" to Bob
    (is (= alice (get (:tempids tx) "A")))
    (is (= bob (get (:tempids tx) "B")))
    ;; e3 refs Alice, e4 refs Bob
    ;; entity-map wraps refs in {:db/id uuid}, so extract :db/id
    (is (= alice (:db/id (:ref (tdc/entity-map (:db-after tx) e3)))))
    (is (= bob (:db/id (:ref (tdc/entity-map (:db-after tx) e4)))))))

(deftest test-vector-upsert
  (let [db* (fn []
              (let [tx (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
                         [{:db/id "ivan", :name "Ivan"}])]
                {:db (:db-after tx)
                 :ivan (get (:tempids tx) "ivan")}))]
    ;; Test that -1 upserts to existing Ivan
    (let [{:keys [db ivan]} (db*)
          tx (d/with db [[:db/add -1 :name "Ivan"]
                         [:db/add -1 :age 12]])]
      (is (= ivan (get (:tempids tx) -1)))
      (is (= {:db/id ivan :name "Ivan" :age 12}
            (tdc/entity-map (:db-after tx) [:name "Ivan"]))))

    ;; Test order doesn't matter
    (let [{:keys [db ivan]} (db*)
          tx (d/with db [[:db/add -1 :age 12]
                         [:db/add -1 :name "Ivan"]])]
      (is (= ivan (get (:tempids tx) -1)))
      (is (= {:db/id ivan :name "Ivan" :age 12}
            (tdc/entity-map (:db-after tx) [:name "Ivan"])))))

  ;; Test conflicting upserts
  (let [tx0 (d/with (d/empty-db {:name {:db/unique :db.unique/identity}})
              [[:db/add "ivan" :name "Ivan"]
               [:db/add "oleg" :name "Oleg"]])
        db (:db-after tx0)]
    (is (thrown-with-msg? Throwable #"Conflicting upsert:.*resolves both to"
          (d/with db [[:db/add -1 :name "Ivan"]
                      [:db/add -1 :age 35]
                      [:db/add -1 :name "Oleg"]
                      [:db/add -1 :age 36]])))))
