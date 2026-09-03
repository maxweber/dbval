(ns dbval.test
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    dbval.test.core

    dbval.test.components
    dbval.test.conn
    dbval.test.db
    dbval.test.entity
    dbval.test.explode
    dbval.test.filter
    dbval.test.ident
    dbval.test.index
    dbval.test.listen
    dbval.test.lookup-refs
    dbval.test.lru
    dbval.test.parser
    dbval.test.parser-find
    dbval.test.parser-return-map
    dbval.test.parser-rules
    dbval.test.parser-query
    dbval.test.parser-where
    dbval.test.pull-api
    dbval.test.pull-parser
    dbval.test.store
    dbval.test.time-travel
    dbval.test.query
    dbval.test.query-aggregates
    dbval.test.query-find-specs
    dbval.test.query-fns
    dbval.test.query-not
    dbval.test.query-or
    dbval.test.query-pull
    dbval.test.query-return-map
    dbval.test.query-rules
    dbval.test.transact
    dbval.test.tuple
    dbval.test.tuples
    dbval.test.validation
    dbval.test.upsert
    dbval.test.issues
    dbval.test.datafy))

(defn ^:export test-clj []
  (dbval.test.core/wrap-res #(t/run-all-tests #"dbval\..*")))


