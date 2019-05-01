(ns mesh-network-clojure.storage
  (:use [clojure.core.async :as async]))

(defrecord storage-driver [channel])

(def driver? (partial instance? storage-driver))

(defn put-data! [driver data]
  (when (driver? driver)
    (async/go (async/>! (:channel driver) data))))
