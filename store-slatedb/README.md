# dbval-slatedb

A [SlateDB](https://slatedb.io)-backed `dbval.store` implementation: an
embedded ordered key-value store built on object storage (S3, GCS, or the
local filesystem during development).

This lives in its own module because `io.slatedb/slatedb-uniffi` is a heavy
native dependency, and `dbval.store.slatedb` cannot even load without it —
consumers on the SQLite or memory backend should not have to download it.

## Usage

Depend on this module (e.g. via a git dependency with `:deps/root
"store-slatedb"`), then:

```clojure
(require '[dbval.core :as d]
         '[dbval.store.slatedb :as slatedb])

(d/empty-db schema {:store (slatedb/store {:db-file          "my-app/db"
                                           :object-store-url "file:///"})})
```

`:object-store-url` accepts `"file:///"`, `"memory:///"`, or an `s3://` URL.

## Requirements

`slatedb-uniffi` is compiled for class-file version 66, so this module
requires **JDK 22 or newer** (dbval itself runs on JDK 11+).

## Tests

```bash
clojure -M:test
```
