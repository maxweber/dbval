(ns ^:no-doc dbval.arrays
  "Object-array helpers. Replaces me.tonsky.persistent-sorted-set.arrays,
   which was dbval's last use of the persistent-sorted-set library."
  (:refer-clojure :exclude [array? make-array aget aset aclone]))

(defn array? [x]
  (some-> ^Object x class .isArray))

(defn make-array ^objects [n]
  (object-array n))

(defn aget [^objects a ^long i]
  (clojure.core/aget a i))

(defn aset [^objects a ^long i v]
  (clojure.core/aset a i v))

(defn aclone ^objects [^objects a]
  (clojure.core/aclone a))
