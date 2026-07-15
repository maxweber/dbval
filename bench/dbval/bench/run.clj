(ns dbval.bench.run
  "Benchmarks for dbval.

   Two kinds of benchmarks:

   - Transaction benchmarks (`add-*`, `init`, `retract-5`) replay all
     transactions against a fresh store on every iteration, because a db
     snapshot cannot be transacted against once its store has moved on.
     They use the small `*people1k` dataset and run with `*batch*` 1.
     Note: every iteration creates a temporary SQLite file.

   - Query/read benchmarks build a store once and only read from it."
  (:require
   [dbval.core :as d]
   [dbval.bench.bench :as bench]))

(def schema
  {:id      {:db/unique :db.unique/identity}
   :follows {:db/valueType   :db.type/ref
             :db/cardinality :db.cardinality/many}
   :alias   {:db/cardinality :db.cardinality/many}})

(def *db20k
  (delay
    (d/db-with (d/empty-db schema) @bench/*people20k)))

(defn wide-db [depth width]
  (d/db-with (d/empty-db schema) (bench/wide-db 1 depth width)))

(defn long-db [depth width]
  (d/db-with (d/empty-db schema) (bench/long-db depth width)))

;; transactions

(defn bench-add-1 []
  (binding [bench/*batch* 1]
    (bench/bench "add-1"
      (reduce
        (fn [db p]
          (-> db
            (d/db-with [[:db/add (:db/id p) :name      (:name p)]])
            (d/db-with [[:db/add (:db/id p) :last-name (:last-name p)]])
            (d/db-with [[:db/add (:db/id p) :sex       (:sex p)]])
            (d/db-with [[:db/add (:db/id p) :age       (:age p)]])
            (d/db-with [[:db/add (:db/id p) :salary    (:salary p)]])))
        (d/empty-db schema)
        @bench/*people1k))))

(defn bench-add-5 []
  (binding [bench/*batch* 1]
    (bench/bench "add-5"
      (reduce
        (fn [db p]
          (d/db-with db [p]))
        (d/empty-db schema)
        @bench/*people1k))))

(defn bench-add-all []
  (binding [bench/*batch* 1]
    (bench/bench "add-all"
      (d/db-with
        (d/empty-db schema)
        @bench/*people1k))))

(defn bench-init []
  (let [eids   (into {}
                 (map (fn [p] [(:db/id p) (random-uuid)]))
                 @bench/*people1k)
        datoms (into []
                 (for [p @bench/*people1k
                       :let [id (eids (:db/id p))]
                       [k v] p
                       :when (not= k :db/id)
                       ;; explode cardinality-many values into datoms,
                       ;; like transacting the map would
                       v (if (sequential? v) v [v])]
                   (d/datom id k v)))]
    (binding [bench/*batch* 1]
      (bench/bench "init"
        (d/init-db datoms)))))

(defn bench-retract-5 []
  ;; includes rebuilding the store: a snapshot cannot be re-retracted once
  ;; its store has moved on
  (binding [bench/*batch* 1]
    (bench/bench "retract-5"
      (let [db   (d/db-with (d/empty-db schema) @bench/*people1k)
            eids (->> (d/datoms db :aevt :name) (map :e) (shuffle))]
        (reduce (fn [db eid] (d/db-with db [[:db.fn/retractEntity eid]])) db eids)))))

;; reads

(def *eids1k
  (delay
    (->> (d/datoms @*db20k :aevt :full-name)
      (map :e)
      (shuffle)
      (take 1000)
      (vec))))

(defn bench-find-datoms []
  (let [db   @*db20k
        eids @*eids1k]
    (bench/bench "find-datoms"
      (doseq [id eids]
        (-> (d/datoms db :eavt id :full-name)
          first
          :v)))))

(defn bench-find-datom []
  (let [db   @*db20k
        eids @*eids1k]
    (bench/bench "find-datom"
      (doseq [id eids]
        (-> (d/find-datom db :eavt id :full-name)
          :v)))))

(defn bench-q1 []
  (bench/bench "q1"
    (d/q '[:find ?e
           :where [?e :name "Ivan"]]
      @*db20k)))

(defn bench-q2 []
  (bench/bench "q2"
    (d/q '[:find ?e ?a
           :where [?e :name "Ivan"]
                  [?e :age ?a]]
      @*db20k)))

(defn bench-q3 []
  (bench/bench "q3"
    (d/q '[:find ?e ?a
           :where [?e :name "Ivan"]
                  [?e :age ?a]
                  [?e :sex :male]]
      @*db20k)))

(defn bench-q4 []
  (bench/bench "q4"
    (d/q '[:find ?e ?l ?a
           :where [?e :name "Ivan"]
                  [?e :last-name ?l]
                  [?e :age ?a]
                  [?e :sex :male]]
      @*db20k)))

(defn bench-q5-shortcircuit []
  (bench/bench "q5-shortcircuit"
    (d/q '[:find ?e ?n ?l ?a ?s ?al
           :in $ ?n ?a
           :where [?e :name ?n]
                  [?e :age ?a]
                  [?e :last-name ?l]
                  [?e :sex ?s]
                  [?e :alias ?al]]
      @*db20k
      "Anastasia"
      35)))

(defn bench-qpred1 []
  (bench/bench "qpred1"
    (d/q '[:find ?e ?s
           :where [?e :salary ?s]
                  [(> ?s 50000)]]
      @*db20k)))

(defn bench-qpred2 []
  (bench/bench "qpred2"
    (d/q '[:find ?e ?s
           :in   $ ?min_s
           :where [?e :salary ?s]
                  [(> ?s ?min_s)]]
      @*db20k 50000)))

;; pull

(def *pull-db
  (delay
    (wide-db 4 5)))

(defn bench-pull-one-entities []
  (let [f (fn f [entity]
            (assoc
              (select-keys entity [:name])
              :follows (mapv f (:follows entity))))]
    (bench/bench "pull-one-entities"
      (f (d/entity @*pull-db [:id 1])))))

(defn bench-pull-one []
  (bench/bench "pull-one"
    (d/pull @*pull-db [:name {:follows '...}] [:id 1])))

(defn bench-pull-many-entities []
  (let [f (fn f [entity]
            (assoc
              (select-keys entity [:db/id :last-name :alias :sex :age :salary])
              :follows (mapv f (:follows entity))))]
    (bench/bench "pull-many-entities"
      (f (d/entity @*pull-db [:id 1])))))

(defn bench-pull-many []
  (bench/bench "pull-many"
    (d/pull @*pull-db [:db/id :last-name :alias :sex :age :salary {:follows '...}] [:id 1])))

(defn bench-pull-wildcard []
  (bench/bench "pull-wildcard"
    (d/pull @*pull-db ['* {:follows '...}] [:id 1])))

;; rules

(defn bench-rules [db]
  (d/q '[:find ?e ?e2
         :in   $ %
         :where (follows ?e ?e2)]
       db
       '[[(follows ?x ?y)
          [?x :follows ?y]]
         [(follows ?x ?y)
          [?x :follows ?t]
          (follows ?t ?y)]]))

(defn bench-rules-wide-3x3 []
  (let [db (wide-db 3 3)]
    (bench/bench "rules-wide-3x3" (bench-rules db))))

(defn bench-rules-wide-5x3 []
  (let [db (wide-db 5 3)]
    (bench/bench "rules-wide-5x3" (bench-rules db))))

(defn bench-rules-wide-7x3 []
  (let [db (wide-db 7 3)]
    (bench/bench "rules-wide-7x3" (bench-rules db))))

(defn bench-rules-wide-4x6 []
  (let [db (wide-db 4 6)]
    (bench/bench "rules-wide-4x6" (bench-rules db))))

(defn bench-rules-long-10x3 []
  (let [db (long-db 10 3)]
    (bench/bench "rules-long-10x3" (bench-rules db))))

(defn bench-rules-long-30x3 []
  (let [db (long-db 30 3)]
    (bench/bench "rules-long-30x3" (bench-rules db))))

(defn bench-rules-long-30x5 []
  (let [db (long-db 30 5)]
    (bench/bench "rules-long-30x5" (bench-rules db))))

(def benches
  {"add-1"              bench-add-1
   "add-5"              bench-add-5
   "add-all"            bench-add-all
   "init"               bench-init
   "find-datoms"        bench-find-datoms
   "find-datom"         bench-find-datom
   "retract-5"          bench-retract-5
   "q1"                 bench-q1
   "q2"                 bench-q2
   "q3"                 bench-q3
   "q4"                 bench-q4
   "q5-shortcircuit"    bench-q5-shortcircuit
   "qpred1"             bench-qpred1
   "qpred2"             bench-qpred2
   "pull-one-entities"  bench-pull-one-entities
   "pull-one"           bench-pull-one
   "pull-many-entities" bench-pull-many-entities
   "pull-many"          bench-pull-many
   "pull-wildcard"      bench-pull-wildcard
   "rules-wide-3x3"     bench-rules-wide-3x3
   "rules-wide-5x3"     bench-rules-wide-5x3
   "rules-wide-7x3"     bench-rules-wide-7x3
   "rules-wide-4x6"     bench-rules-wide-4x6
   "rules-long-10x3"    bench-rules-long-10x3
   "rules-long-30x3"    bench-rules-long-30x3
   "rules-long-30x5"    bench-rules-long-30x5})

(defn ^:export -main
  "./script/bench.sh [--profile] (add-1 | add-5 | ...)*"
  [& args]
  (let [args     (or args ())
        profile? (.contains ^java.util.List args "--profile")
        args     (remove #{"--profile"} args)
        names    (or (not-empty args) (sort (keys benches)))
        _        (apply println "Benchmarks:" names)
        longest  (last (sort-by count names))]
    (binding [bench/*profile* profile?]
      (doseq [name names
              :let [fn (benches name)]]
        (if (nil? fn)
          (println "Unknown benchmark:" name)
          (let [{:keys [mean-ms file]} (fn)]
            (println
              (bench/right-pad name (count longest))
              " "
              (bench/left-pad (bench/round mean-ms) 8) "ms/op"
              " " (or file ""))))))
    (shutdown-agents)))
