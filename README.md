# dbval

dbval is a fork of [Datascript](https://github.com/tonsky/datascript) and a
proof-of-concept (aka 'do not use it in production') that you can implement a
library that offers Datomic-like semantics on top of a mutable relational
database like Sqlite.

The most important goal is to serve the database as a value, meaning you can get
the current database value and query it as long as you like without that it
changes underneath you. You can also get the database as a value for any point
in the past.

Sqlite was chosen since you have no network-round trip when you read from the
database. Thereby you can do [hundreds of small
queries](https://www.sqlite.org/np1queryprob.html) instead of trying to force
everything into one big SQL statement.

The idea to have something Datomic-like on top of a relational database was
already born in 2013 during a project where we were forced to use Postgres
instead of Datomic. It was a side quest ever since. We
([@DerGuteMoritz](https://github.com/dergutemoritz) and
[me](https://github.com/maxweber)) tried many different database schemas over
the years. In 2025 a breakthrough were accomplished after learning about
FoundationDB and that [Griffin already has a fork of Datascript that runs on top
of FoundationDB](https://www.juxt.pro/blog/clojure-in-griffin/#foundationdb).

While FoundationDB is an amazing piece of technology it requires a lot of
infrastructure. For us mortals there is basically only the option to use its
Kubernetes operator. Meanwhile the [Rails
community](https://m.youtube.com/watch?v=Sc4FJ0EZTAg) and projects like
[Turso](https://turso.tech/) proofed that it is viable to have one Sqlite
database per (SaaS) customer.

At its core FoundationDB is a transactional ordered key value store. Something
you can mimic in Sqlite with a table like:

    create table dbval (k blob not null, primary key(k)) WITHOUT ROWID;

dbval only needs the key portion. Consequently, you are dealing with a sorted
set and Datascript's core is a
[persistent-sorted-set](https://github.com/tonsky/persistent-sorted-set).

The storage layer is pluggable via the `dbval.store` protocol: a store only
has to provide ordered range scans over committed keys and atomic batch
commits. `dbval.store.sqlite` is the default backend,
`dbval.store.memory` provides an in-memory store for tests, and the
[store-slatedb](store-slatedb/) module provides a
[SlateDB](https://slatedb.io) backend on object storage. Note that dbval
ships no storage driver — to use the default SQLite store, add
`org.xerial/sqlite-jdbc` to your dependencies (like with
[next.jdbc](https://github.com/seancorfield/next-jdbc), you bring the driver
for the store you pick).

I first assumed that I need to make one part of a datom mutable, so that I can
mark it as retracted. Until I discovered that the sorting of `t` in the Datomic
indexes `:eavt`, `:aevt`, `:avet` and `:vaet` allows to figure out what the
current state was at a given point in time, so that you can serve an immutable
database value. The secret sauce can be found in the `dbval.db/datoms-filter`
transducer.

One obvious downside is that the database will keep growing forever, potentially
making `dbval.db/datoms-filter` slower and slower over time. However, from one
of the many Datomic talks I learned a nice trick to mitigate this. You can have
one table that contain the complete history, while another one might only
contain the history of the last 30 days. Consequently, you need to query the
former table for older historic database values, while the latter will stay
smaller and is faster to query.

Back to FoundationDB, its keys and values are just byte arrays. The key contains
a tuple and its byte array representation allows to sort it, even if it is a mix
of different types (String, double, UUID, nested tuples, etc.). You will notice
that you can represent a (Datomic) datom as a tuple. Luckily, the tuple to byte
array logic is available via [Tuple
class](https://apple.github.io/foundationdb/javadoc/com/apple/foundationdb/tuple/Tuple.html).

One question that might arise is how you can have different indexes in a
FoundationDB-like model. As you can see from the `create table` SQL statement
above we only have a single column (with a btree index on it). In
[FoundationDB](https://apple.github.io/foundationdb/simple-indexes-java.html)
you use a prefix to differentiate between indexes. You can think of a bit like
subfolders on your file system. Let's assume we have the following datom:

    [123 :language "Clojure" 1001 true]

Then dbval maintains the following indexes, by inserting the corresponding
tuples into the dbval table:

| 0       | 1        | 2        | 3         | 4        | 5     |
|---------|----------|----------|-----------|----------|-------|
| "eavt"  | 123      | :language| "Clojure" | 1001     | true  |
| "aevt"  | :language| 123      | "Clojure" | 1001     | true  |
| "avet"  | :language| "Clojure"| 123       | 1001     | true  |
| "teav"  | 1001     | 123      | :language | "Clojure"| true  |


The last boolean indicates if the Datom was added or retracted. Like Datascript
dbval does not maintain a "vaet" index (like Datomic does). Additionally dbval
maintains the "teav" index that can be used to efficiently retrieve the most
recent transaction.

Our current SaaS runs Datomic in production since 2018. Overall we are happy
with it. The biggest challenge for us were large migrations that have to be
split into many smaller transactions, to avoid that the transactor is occupied
for too long. Otherwise customers have to wait a couple of seconds before their
write is processed. The next generation of our SaaS runs one logical Datomic
database per customer (with a shared transactor pair) to avoid this issue. Your
challenges may vary but I think it's important that open source alternatives to
Datomic exist, so that you have the option to make different trade-offs.

Luckily there are already a couple of Datomic open source alternatives. Why
another one? Some does not offer you the database as a value. But the key is
that dbval tries to pick a minimal scope, since implementing a database almost
from scratch is a humongous task. For that reason dbval considers itself as a
database library and only tries to marry Datascript with Sqlite. Most
database-related features are already solved by Sqlite or its
[ecosystem](https://litestream.io/).

As Datascript's creator Nikita Prokopov states in his blog post [Ideas for
DataScript 2](https://tonsky.me/blog/datascript-2/):

> UUIDs for entity IDs makes it easier to generate new IDs in distributed
> environment without consulting central authority.

dbval uses UUIDs for entity IDs. The biggest motivator is to avoid the need to
assign an external ID to each entity. In past we often made the mistake to share
Datomic entity IDs with the outside world (via an API for example), while this
is strictly discouraged. In Datomic and Datascript each transaction also receive
its own entity ID. dbval uses
[colossal-squuid](https://github.com/yetanalytics/colossal-squuid) UUIDs for
transaction entity IDs. They increase strictly monotonically, meaning:

> A SQUUID generated later will always have a higher value, both
> lexicographically and in terms of the underlying bits, than one generated
> earlier.

With `com.yetanalytics.squuid/uuid->time` you can extract the timestamp that is
encoded in the leading bits of the SQUUID:

```clojure

(uuid->time #uuid "017de28f-5801-8fff-8fff-ffffffffffff")
;; => #inst "2021-12-22T14:33:04.769000000-00:00"

```

This timestamp can serve as `:db/txInstant` to capture when the transaction has
been transacted. UUIDs for entity and transaction IDs would allow to entirely
get rid of tempids. However, they are still supported by dbval for convenience
and to assign data to the transaction entity:

``` clojure
(d/transact! conn
  [[:db/add "e1" :name "Alice"]

   ;; attach metadata to the transaction
   [:db/add :db/current-tx :tx/user-id 42]
   [:db/add :db/current-tx :tx/source :api]])
```

Another compelling option of using UUIDs is that dbval databases become
mergeable, if they adhere to the same schema. Thereby you can solve the
following challenge: if you have a separate database per customer it is no
longer possible to run database queries to get statistics across your customer
base. With dbval you can merge all customer databases into a big one to run
these statistics queries.

One obvious downside of UUIDs is that they need twice as much storage in
comparison to 64 bit integers.

## Quickstart

At the moment the project is a proof-of-concept and not meant to be used in
real-world applications. However, if you want to hack on it a good starting
point is to run the unit tests via:

    script/test_clj.sh

## TODOs

- Mature the library into something 'production-ready'

- dbval should add a `:db/txInstant` to each transaction entity with a
  `java.util.Date` of when the transaction was transacted.

- Add an equivalent to `datomic.api/as-of`

- Better connection management

- Also adapt the ClojureScript parts (broken at the moment).

    - There is [a JS libary](https://github.com/josephg/fdb-tuple) that implements the FoundationDB tuple encoding.
    
    - Doing [synchronous SQLite reads and writes in JavaScript](https://blog.cloudflare.com/sqlite-in-durable-objects/#reads-and-writes-are-synchronous) is viable (no need to make everything async).

- Consider to increase `tx0`, `emax` and `txmax`

- Build an example application app with dbval + Sqlite as a database to check if
  something is missing.
