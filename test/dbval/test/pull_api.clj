(ns dbval.test.pull-api
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [dbval.core :as d]
    [dbval.db :as db]
    [dbval.test.core :as tdc]))

(def ^:private test-schema
  {:name   {:db/unique :db.unique/identity}
   :aka    {:db/cardinality :db.cardinality/many}
   :child  {:db/cardinality :db.cardinality/many
            :db/valueType :db.type/ref}
   :friend {:db/cardinality :db.cardinality/many
            :db/valueType :db.type/ref}
   :enemy  {:db/cardinality :db.cardinality/many
            :db/valueType :db.type/ref}
   :father {:db/valueType :db.type/ref}

   :part   {:db/valueType :db.type/ref
            :db/isComponent true
            :db/cardinality :db.cardinality/many}
   :spec   {:db/valueType :db.type/ref
            :db/isComponent true
            :db/cardinality :db.cardinality/one}})

;; Build test database with entity maps instead of raw datoms
(defn- make-test-db []
  (let [tx (d/with (d/empty-db test-schema)
             [;; People
              {:db/id "petr"     :name "Petr" :aka ["Devil" "Tupen"]}
              {:db/id "david"    :name "David"}
              {:db/id "thomas"   :name "Thomas"}
              {:db/id "lucy"     :name "Lucy"}
              {:db/id "elizabeth" :name "Elizabeth"}
              {:db/id "matthew"  :name "Matthew"}
              {:db/id "eunan"    :name "Eunan"}
              {:db/id "kerri"    :name "Kerri"}
              {:db/id "rebecca"  :name "Rebecca"}
              ;; Relationships
              [:db/add "petr" :child "david"]
              [:db/add "petr" :child "thomas"]
              [:db/add "david" :father "petr"]
              [:db/add "thomas" :father "petr"]
              [:db/add "matthew" :father "thomas"]
              ;; Parts hierarchy
              {:db/id "parta"     :name "Part A"}
              {:db/id "partaa"    :name "Part A.A"}
              {:db/id "partaaa"   :name "Part A.A.A"}
              {:db/id "partaaaa"  :name "Part A.A.A.A"}
              {:db/id "partaaab"  :name "Part A.A.A.B"}
              {:db/id "partab"    :name "Part A.B"}
              {:db/id "partaba"   :name "Part A.B.A"}
              {:db/id "partabaa"  :name "Part A.B.A.A"}
              {:db/id "partabab"  :name "Part A.B.A.B"}
              [:db/add "parta" :part "partaa"]
              [:db/add "partaa" :part "partaaa"]
              [:db/add "partaaa" :part "partaaaa"]
              [:db/add "partaaa" :part "partaaab"]
              [:db/add "parta" :part "partab"]
              [:db/add "partab" :part "partaba"]
              [:db/add "partaba" :part "partabaa"]
              [:db/add "partaba" :part "partabab"]])]
    {:db (:db-after tx)
     :tempids (:tempids tx)}))

(def ^:private *test-data (delay (make-test-db)))
(defn- test-db [] (:db @*test-data))
(defn- eid [name] (get (:tempids @*test-data) name))

(deftest test-pull-attr-spec
  (is (= {:name "Petr" :aka ["Devil" "Tupen"]}
        (d/pull (test-db) '[:name :aka] [:name "Petr"])))

  (is (= {:name "Matthew" :father {:db/id (eid "thomas")} :db/id (eid "matthew")}
        (d/pull (test-db) '[:name :father :db/id] [:name "Matthew"])))

  (is (= [{:name "Petr"} {:name "Elizabeth"}
          {:name "Eunan"} {:name "Rebecca"}]
        (d/pull-many (test-db) '[:name] [[:name "Petr"] [:name "Elizabeth"]
                                         [:name "Eunan"] [:name "Rebecca"]]))))

(deftest test-pull-reverse-attr-spec
  (is (= {:name "David" :_child [{:db/id (eid "petr")}]}
        (d/pull (test-db) '[:name :_child] [:name "David"])))

  (is (= {:name "David" :_child [{:name "Petr"}]}
        (d/pull (test-db) '[:name {:_child [:name]}] [:name "David"])))

  (testing "Reverse non-component references yield collections"
    (is (= {:name "Thomas" :_father [{:db/id (eid "matthew")}]}
          (d/pull (test-db) '[:name :_father] [:name "Thomas"])))

    (is (= {:name "Petr" :_father #{(set [{:db/id (eid "david")} {:db/id (eid "thomas")}])}}
          ;; Check set equality since order may vary
          (let [result (d/pull (test-db) '[:name :_father] [:name "Petr"])]
            {:name (:name result)
             :_father #{(set (:_father result))}})))

    (is (= {:name "Thomas" :_father [{:name "Matthew"}]}
          (d/pull (test-db) '[:name {:_father [:name]}] [:name "Thomas"])))

    (is (= #{{:name "David"} {:name "Thomas"}}
          (set (:_father (d/pull (test-db) '[:name {:_father [:name]}] [:name "Petr"]))))))

  (testing "Multiple reverse refs issue-412"
    (is (= #{{:db/id (eid "david")} {:db/id (eid "thomas")}}
          (set (:_father (d/pull (test-db) '[:name :_father :_child] [:name "Petr"])))))))

(deftest test-pull-component-attr
  (testing "Component entities are expanded recursively"
    (let [result (d/pull (test-db) '[:name :part] [:name "Part A"])]
      (is (= "Part A" (:name result)))
      (is (= 2 (count (:part result))))
      ;; Check nested structure
      (is (= #{"Part A.A" "Part A.B"}
             (set (map :name (:part result)))))))

  (testing "Reverse component references yield a single result"
    (is (= {:name "Part A.A" :_part {:db/id (eid "parta")}}
          (d/pull (test-db) [:name :_part] [:name "Part A.A"])))

    (is (= {:name "Part A.A" :_part {:name "Part A"}}
          (d/pull (test-db) [:name {:_part [:name]}] [:name "Part A.A"]))))

  (testing "Reverse recursive component issue-411"
    (is (= {:name "Part A.A.A.B" :_part {:name "Part A.A.A" :_part {:name "Part A.A" :_part {:name "Part A"}}}}
          (d/pull (test-db) '[:name {:_part ...}] [:name "Part A.A.A.B"])))
    (is (= {:name "Part A.A.A.B" :_part {:name "Part A.A.A" :_part {:name "Part A.A"}}}
          (d/pull (test-db) '[:name {:_part 2}] [:name "Part A.A.A.B"])))))

(deftest test-pull-wildcard
  (let [petr (eid "petr")
        david (eid "david")
        thomas (eid "thomas")]
    (testing "Wildcard pulls all attributes"
      (let [result (d/pull (test-db) '[*] [:name "Petr"])]
        (is (= petr (:db/id result)))
        (is (= "Petr" (:name result)))
        (is (= ["Devil" "Tupen"] (:aka result)))
        (is (= 2 (count (:child result))))))

    (testing "Wildcard with reverse ref"
      (let [result (d/pull (test-db) '[* :_child] [:name "David"])]
        (is (= "David" (:name result)))
        (is (= {:db/id petr} (:father result)))
        (is (= [{:db/id petr}] (:_child result)))))

    (testing "Wildcard with explicit attrs"
      (let [result (d/pull (test-db) '[:name *] [:name "Petr"])]
        (is (= "Petr" (:name result)))
        (is (= ["Devil" "Tupen"] (:aka result)))))

    (testing "Wildcard with :as aliases"
      (let [result (d/pull (test-db) '[[:aka :as :alias] [:name :as :first-name] *] [:name "Petr"])]
        (is (= "Petr" (:first-name result)))
        (is (= ["Devil" "Tupen"] (:alias result)))))

    (testing "Wildcard with recursive child expansion"
      (let [result (d/pull (test-db) '[* {:child ...}] [:name "Petr"])]
        (is (= "Petr" (:name result)))
        (is (= 2 (count (:child result))))
        (is (= #{"David" "Thomas"} (set (map :name (:child result)))))))))

(deftest test-pull-limit
  ;; Create a test db with many akas
  (let [tx (d/with (d/empty-db test-schema)
             (concat
               [{:db/id "lucy" :name "Lucy"
                 :friend ["elizabeth" "matthew" "eunan" "kerri"]}
                {:db/id "elizabeth" :name "Elizabeth"}
                {:db/id "matthew" :name "Matthew"}
                {:db/id "eunan" :name "Eunan"}
                {:db/id "kerri" :name "Kerri" :aka (vec (for [idx (range 2000)] (str "aka-" idx)))}]))
        db (:db-after tx)]

    (testing "Without an explicit limit, the default is 1000"
      (is (= 1000 (->> (d/pull db '[:aka] [:name "Kerri"]) :aka count))))

    (testing "Explicit limit can reduce the default"
      (is (= 500 (->> (d/pull db '[(limit :aka 500)] [:name "Kerri"]) :aka count)))
      (is (= 500 (->> (d/pull db '[[:aka :limit 500]] [:name "Kerri"]) :aka count))))

    (testing "Explicit limit can increase the default"
      (is (= 1500 (->> (d/pull db '[(limit :aka 1500)] [:name "Kerri"]) :aka count))))

    (testing "A nil limit produces unlimited results"
      (is (= 2000 (->> (d/pull db '[(limit :aka nil)] [:name "Kerri"]) :aka count))))

    (testing "Limits can be used as map specification keys"
      (let [result (d/pull db '[:name {(limit :friend 2) [:name]}] [:name "Lucy"])]
        (is (= "Lucy" (:name result)))
        (is (= 2 (count (:friend result))))))))

(deftest test-pull-default
  (testing "Empty results return nil"
    (is (nil? (d/pull (test-db) '[:foo] [:name "Petr"]))))

  (testing "A default can be used to replace nil results"
    (is (= {:foo "bar"}
          (d/pull (test-db) '[(default :foo "bar")] [:name "Petr"])))
    (is (= {:foo "bar"}
          (d/pull (test-db) '[[:foo :default "bar"]] [:name "Petr"])))
    (is (= {:foo false}
          (d/pull (test-db) '[[:foo :default false]] [:name "Petr"])))
    (is (= {:bar false}
          (d/pull (test-db) '[[:foo :as :bar :default false]] [:name "Petr"]))))

  (testing "default does not override results"
    ;; Children order is not deterministic with UUIDs
    (let [result (d/pull (test-db)
                   '[[:name :default "[name]"]
                     [:aka :default "[aka]"]
                     {[:child :default "[child]"] ...}]
                   [:name "Petr"])]
      (is (= "Petr" (:name result)))
      (is (= ["Devil" "Tupen"] (:aka result)))
      (is (= #{{:name "David", :aka "[aka]", :child "[child]"}
               {:name "Thomas", :aka "[aka]", :child "[child]"}}
            (set (:child result)))))
    (is (= {:name "David", :aka  "[aka]", :child "[child]"}
          (d/pull (test-db)
            '[[:name :default "[name]"]
              [:aka :default "[aka]"]
              {[:child :default "[child]"] ...}]
            [:name "David"]))))

  (testing "Ref default"
    (is (= {:child 1 :db/id (eid "david")}
          (d/pull (test-db) '[:db/id [:child :default 1]] [:name "David"])))
    (is (= {:_child 2 :db/id (eid "petr")}
          (d/pull (test-db) '[:db/id [:_child :default 2]] [:name "Petr"])))))

(deftest test-pull-as
  (is (= {"Name" "Petr", :alias ["Devil" "Tupen"]}
        (d/pull (test-db) '[[:name :as "Name"] [:aka :as :alias]] [:name "Petr"]))))

(deftest test-pull-attr-with-opts
  (is (= {"Name" "Nothing"}
        (d/pull (test-db) '[[:x :as "Name" :default "Nothing"]] [:name "Petr"]))))

(deftest test-pull-map
  (testing "Single attrs yield a map"
    (is (= {:name "Matthew" :father {:name "Thomas"}}
          (d/pull (test-db) '[:name {:father [:name]}] [:name "Matthew"]))))

  (testing "Multi attrs yield a collection of maps"
    ;; Children order is not deterministic with UUIDs
    (let [result (d/pull (test-db) '[:name {:child [:name]}] [:name "Petr"])]
      (is (= "Petr" (:name result)))
      (is (= #{{:name "David"} {:name "Thomas"}}
            (set (:child result))))))

  (testing "Missing attrs are dropped"
    (is (= {:name "Petr"}
          (d/pull (test-db) '[:name {:father [:name]}] [:name "Petr"]))))

  (testing "Non matching results are removed from collections"
    (is (= {:name "Petr"}
          (d/pull (test-db) '[:name {:child [:foo]}] [:name "Petr"]))))

  (testing "Map specs can override component expansion"
    ;; Part order is not deterministic with UUIDs, use set comparison
    (let [result (d/pull (test-db) '[:name {:part [:name]}] [:name "Part A"])]
      (is (= "Part A" (:name result)))
      (is (= #{{:name "Part A.A"} {:name "Part A.B"}} (set (:part result)))))

    (let [result (d/pull (test-db) '[:name {:part 1}] [:name "Part A"])]
      (is (= "Part A" (:name result)))
      (is (= #{{:name "Part A.A"} {:name "Part A.B"}} (set (:part result)))))))

(deftest test-pull-recursion
  (let [lucy (eid "lucy")
        elizabeth (eid "elizabeth")
        matthew (eid "matthew")
        eunan (eid "eunan")
        kerri (eid "kerri")
        db      (-> (test-db)
                  (d/db-with [[:db/add [:name "Lucy"] :friend [:name "Elizabeth"]]
                              [:db/add [:name "Elizabeth"] :friend [:name "Matthew"]]
                              [:db/add [:name "Matthew"] :friend [:name "Eunan"]]
                              [:db/add [:name "Eunan"] :friend [:name "Kerri"]]
                              [:db/add [:name "Lucy"] :enemy [:name "Matthew"]]
                              [:db/add [:name "Elizabeth"] :enemy [:name "Eunan"]]
                              [:db/add [:name "Matthew"] :enemy [:name "Kerri"]]
                              [:db/add [:name "Eunan"] :enemy [:name "Lucy"]]]))
        friends {:db/id lucy
                 :name "Lucy"
                 :friend
                 [{:db/id elizabeth
                   :name "Elizabeth"
                   :friend
                   [{:db/id matthew
                     :name "Matthew"
                     :friend
                     [{:db/id eunan
                       :name "Eunan"
                       :friend
                       [{:db/id kerri
                         :name "Kerri"}]}]}]}]}
        enemies {:db/id lucy
                 :name "Lucy"
                 :friend
                 [{:db/id elizabeth
                   :name "Elizabeth"
                   :friend
                   [{:db/id matthew
                     :name "Matthew"
                     :enemy [{:db/id kerri :name "Kerri"}]}]
                   :enemy
                   [{:db/id eunan
                     :name "Eunan"
                     :friend
                     [{:db/id kerri
                       :name "Kerri"}]
                     :enemy
                     [{:db/id lucy
                       :name "Lucy"
                       :friend [{:db/id elizabeth}]}]}]}]
                 :enemy
                 [{:db/id matthew
                   :name "Matthew"
                   :friend
                   [{:db/id eunan
                     :name "Eunan"
                     :friend
                     [{:db/id kerri
                       :name "Kerri"}]
                     :enemy [{:db/id lucy
                              :name "Lucy"
                              :enemy [{:db/id matthew}]
                              :friend [{:db/id elizabeth
                                        :name "Elizabeth"
                                        :enemy [{:db/id eunan}],
                                        :friend [{:db/id matthew}]}]}]}]
                   :enemy
                   [{:db/id kerri
                     :name "Kerri"}]}]}]

    (testing "Infinite recursion"
      (is (= friends (d/pull db '[:db/id :name {:friend ...}] [:name "Lucy"]))))

    (testing "Multiple recursion specs in one pattern"
      (is (= enemies (d/pull db '[:db/id :name {:friend 2 :enemy 2}] [:name "Lucy"]))))

    (testing "Reverse recursion"
      (is (= {:db/id kerri, :_friend [{:db/id eunan, :_friend [{:db/id matthew, :_friend [{:db/id elizabeth, :_friend [{:db/id lucy}]}]}]}]}
            (d/pull db '[:db/id {:_friend ...}] [:name "Kerri"])))
      (is (= {:db/id kerri, :_friend [{:db/id eunan, :_friend [{:db/id matthew}]}]}
            (d/pull db '[:db/id {:_friend 2}] [:name "Kerri"]))))

    (testing "Cycles are handled by returning only the :db/id of entities which have been seen before"
      (let [db (d/db-with db [[:db/add [:name "Kerri"] :friend [:name "Lucy"]]])]
        (is (= (update-in friends (take 8 (cycle [:friend 0]))
                 assoc :friend [{:db/id lucy :name "Lucy" :friend [{:db/id elizabeth}]}])
              (d/pull db '[:db/id :name {:friend ...}] [:name "Lucy"])))))

    (testing "Seen ids are tracked independently for different branches"
      (let [tx (d/with (d/empty-db {:name {:db/unique :db.unique/identity}
                                    :friend {:db/valueType :db.type/ref}
                                    :enemy  {:db/valueType :db.type/ref}})
                       [{:db/id "e1" :name "1" :friend "e2" :enemy "e2"}
                        {:db/id "e2" :name "2"}])
            db (:db-after tx)]
        (is (= {:name "1" :friend {:name "2"} :enemy {:name "2"}}
              (d/pull db '[:name {:friend [:name], :enemy [:name]}] [:name "1"])))))))

(deftest test-dual-recursion
  (let [tx (d/with (d/empty-db {:friend {:db/valueType :db.type/ref}
                                :enemy {:db/valueType :db.type/ref}})
                   [{:db/id "e1" :friend "e2"}
                    {:db/id "e2" :enemy "e3"}
                    {:db/id "e3" :friend "e4"}
                    {:db/id "e4" :enemy "e5"}
                    {:db/id "e5" :friend "e6"}
                    {:db/id "e6" :enemy "e7"}
                    {:db/id "e7"}])
        recursion-db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")
        e4 (get (:tempids tx) "e4")
        e5 (get (:tempids tx) "e5")]
    (is (= {:db/id e1 :friend {:db/id e2}}
          (d/pull recursion-db '[:db/id {:friend ...}] e1)))
    (is (= {:db/id e1 :friend {:db/id e2 :enemy {:db/id e3}}}
          (d/pull recursion-db '[:db/id {:friend 1 :enemy 1}] e1)))
    (is (= {:db/id e1 :friend {:db/id e2 :enemy {:db/id e3 :friend {:db/id e4}}}}
          (d/pull recursion-db '[:db/id {:friend 2 :enemy 1}] e1)))
    (is (= {:db/id e1 :friend {:db/id e2 :enemy {:db/id e3 :friend {:db/id e4 :enemy {:db/id e5}}}}}
          (d/pull recursion-db '[:db/id {:friend 2 :enemy 2}] e1))))

  (let [tx (d/with (d/empty-db {:part {:db/valueType :db.type/ref}
                                :spec {:db/valueType :db.type/ref}})
                   [{:db/id "e1" :part "e2" :spec "e2"}
                    {:db/id "e2" :part "e3" :spec "e1"}
                    {:db/id "e3" :part "e1"}])
        db (:db-after tx)
        e1 (get (:tempids tx) "e1")
        e2 (get (:tempids tx) "e2")
        e3 (get (:tempids tx) "e3")]
    (is (= (d/pull db '[:db/id {:part ...} {:spec ...}] e1)
          {:db/id e1,
           :spec {:db/id e2
                  :spec {:db/id e1,
                         :spec {:db/id e2}, :part {:db/id e2}}
                  :part {:db/id e3,
                         :part {:db/id e1,
                                :spec {:db/id e2},
                                :part {:db/id e2}}}}
           :part {:db/id e2
                  :spec {:db/id e1, :spec {:db/id e2}, :part {:db/id e2}}
                  :part {:db/id e3,
                         :part {:db/id e1,
                                :spec {:db/id e2},
                                :part {:db/id e2}}}}}))))

(deftest test-deep-recursion
  ;; Test deep recursion with a chain of people
  (let [depth 100  ;; Use smaller depth for UUID-based IDs
        ;; Build chain: Person-0 -> Person-1 -> ... -> Person-99
        entities (into []
                   (for [idx (range depth)]
                     (cond-> {:db/id (str "e" idx) :name (str "Person-" idx)}
                       (< idx (dec depth)) (assoc :friend (str "e" (inc idx))))))
        tx (d/with (d/empty-db {:name {:db/unique :db.unique/identity}
                                :friend {:db/valueType :db.type/ref}}) entities)
        db (:db-after tx)
        pulled (d/pull db '[:name {:friend ...}] [:name "Person-0"])
        ;; :friend is cardinality/one so path is just :friend repeated, not [:friend 0]
        path   (repeat (dec depth) :friend)]
    (is (= (str "Person-" (dec depth))
          (:name (get-in pulled path))))))

; issue-430
(deftest test-component-reverse
  (let [schema {:name {:db/unique :db.unique/identity}
                :ref  {:db/valueType :db.type/ref
                       :db/isComponent true}}
        db (d/db-with (d/empty-db schema)
             [{:name "1"
               :ref {:name "2"
                     :ref {:name "3"}}}])]
    (is (= {:name "1", :ref {:name "2", :ref {:name "3", :_ref {:name "2"}}}}
          (d/pull db
            [:name {:ref [:name {:ref [:name {:_ref [:name]}]}]}]
            [:name "1"])))))

(deftest test-lookup-ref-pull
  (is (= {:name "Petr" :aka ["Devil" "Tupen"]}
        (d/pull (test-db) '[:name :aka] [:name "Petr"])))
  (is (= nil
        (d/pull (test-db) '[:name :aka] [:name "NotInDatabase"])))
  (is (= [nil {:aka ["Devil" "Tupen"]} nil nil nil]
        (d/pull-many (test-db)
          '[:aka]
          [[:name "Elizabeth"]
           [:name "Petr"]
           [:name "Eunan"]
           [:name "Rebecca"]
           [:name "Unknown"]])))
  (is (nil? (d/pull (test-db) '[*] [:name "No such name"]))))

(deftest test-xform
  (let [petr (eid "petr")
        david (eid "david")
        thomas (eid "thomas")]
    ;; Children order is not deterministic with UUIDs, so compare sets
    (let [result (d/pull (test-db)
                   [[:db/id :xform vector]
                    [:name :xform vector]
                    [:aka :xform vector]
                    {[:child :xform vector] '...}]
                   [:name "Petr"])]
      (is (= [petr] (:db/id result)))
      (is (= ["Petr"] (:name result)))
      (is (= [["Devil" "Tupen"]] (:aka result)))
      (is (= #{{:db/id [david], :name ["David"],  :aka [nil], :child [nil]}
               {:db/id [thomas], :name ["Thomas"], :aka [nil], :child [nil]}}
            (set (first (:child result))))))

    (testing ":xform on cardinality/one ref issue-455"
      (is (= {:name "David" :father "Petr"}
            (d/pull (test-db) [:name {[:father :xform #(:name %)] ['*]}] [:name "David"]))))

    (testing ":xform on reverse ref"
      ;; Order is not deterministic with UUIDs
      (let [result (d/pull (test-db) [:name {[:_father :xform #(mapv :name %)] [:name]}] [:name "Petr"])]
        (is (= "Petr" (:name result)))
        (is (= #{"David" "Thomas"} (set (:_father result))))))

    (testing ":xform on reverse component ref"
      (is (= {:name "Part A.A" :_part "Part A"}
            (d/pull (test-db) [:name {[:_part :xform #(:name %)] [:name]}] [:name "Part A.A"]))))

    (testing "missing attrs are processed by xform"
      (is (= {:normal [nil]
              :aka [nil]
              :child [nil]}
            (d/pull (test-db)
              '[[:normal :xform vector]
                [:aka :xform vector]
                {[:child :xform vector] ...}]
              [:name "David"]))))
    (testing "default takes precedence"
      (is (= {:unknown "[unknown]"}
            (d/pull (test-db) '[[:unknown :default "[unknown]" :xform vector]] [:name "Petr"]))))))

(deftest test-visitor
  (let [petr (eid "petr")
        david (eid "david")
        thomas (eid "thomas")
        *trace (volatile! nil)
        opts   {:visitor (fn [k e a v] (vswap! *trace conj [k e a v]))}
        test-fn (fn [pattern id]
                  (vreset! *trace [])
                  (d/pull (test-db) pattern id opts)
                  @*trace)]
    (is (= [[:db.pull/attr petr :name nil]]
          (test-fn [:name] [:name "Petr"])))

    (testing "multival"
      (is (= [[:db.pull/attr petr :aka  nil]
              [:db.pull/attr petr :name nil]]
            (test-fn [:name :aka] [:name "Petr"]))))

    (testing ":db/id is ignored"
      (is (= [] (test-fn [:db/id] [:name "Petr"])))
      (is (= [[:db.pull/attr petr :name nil]]
            (test-fn [:db/id :name] [:name "Petr"]))))

    (testing "wildcard"
      (is (= [[:db.pull/wildcard petr nil    nil]
              [:db.pull/attr     petr :aka   nil]
              [:db.pull/attr     petr :child nil]
              [:db.pull/attr     petr :name  nil]]
            (test-fn ['*] [:name "Petr"]))))

    (testing "missing"
      (is (= [[:db.pull/attr petr :missing nil]]
            (test-fn [:missing] [:name "Petr"])))
      (is (= [[:db.pull/wildcard petr nil      nil]
              [:db.pull/attr     petr :aka     nil]
              [:db.pull/attr     petr :child   nil]
              [:db.pull/attr     petr :missing nil]
              [:db.pull/attr     petr :name    nil]]
            (test-fn ['* :missing] [:name "Petr"]))))

    (testing "default"
      (is (= [[:db.pull/attr petr :missing nil]]
            (test-fn [[:missing :default 10]] [:name "Petr"])))
      (is (= [[:db.pull/attr david :child nil]]
            (test-fn [[:child :default 10]] [:name "David"]))))

    (testing "recursion"
      (is (= [[:db.pull/attr petr :child nil]]
            (test-fn [:child] [:name "Petr"])))
      ;; Children order is not deterministic with UUIDs
      (is (= #{[:db.pull/attr petr :child nil]
               [:db.pull/attr david :name  nil]
               [:db.pull/attr thomas :name  nil]}
            (set (test-fn [{:child [:name]}] [:name "Petr"]))))
      (is (= #{[:db.pull/attr petr :child nil]
               [:db.pull/attr david :child nil]
               [:db.pull/attr david :name  nil]
               [:db.pull/attr thomas :child nil]
               [:db.pull/attr thomas :name  nil]
               [:db.pull/attr petr :name  nil]}
            (set (test-fn [:name {:child '...}] [:name "Petr"])))))

    (testing "reverse"
      (is (= [[:db.pull/attr    david   :name  nil]
              [:db.pull/reverse nil :child david]]
            (test-fn [:name :_child] [:name "David"]))))))

(deftest test-pull-other-dbs
  (let [db (-> (test-db)
             (d/filter (fn [_ datom] (not= "Tupen" (:v datom)))))]
    (is (= {:name "Petr" :aka ["Devil"]}
          (d/pull db '[:name :aka] [:name "Petr"]))))
  (let [db (d/init-db (d/datoms (test-db) :eavt) test-schema)]
    (is (= {:name "Petr" :aka ["Devil" "Tupen"]}
          (d/pull db '[:name :aka] [:name "Petr"])))))
