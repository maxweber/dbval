#!/bin/bash
cd "`dirname $0`/.."

clojure -A:bench -M -m dbval.bench.run $@
