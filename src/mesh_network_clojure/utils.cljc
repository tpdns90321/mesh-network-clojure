(ns mesh-network-clojure.utils)

(defn bytes! [data]
  (if (and (seq? data) (every? #(and (number? %) (< % 256)) data)) data nil))

(defn slice [ar start end]
  (let
    [front (first (split-at end ar))]
    (last (split-at start front))))

(defrecord cache [data limit handler])

(defn create-cache
  ([limit handler] (when (integer? limit) (cache. (ref {}) limit handler)))
  ([limit] (create-cache limit nil))
  ([] (create-cache 100)))

(defn add-cache [cache ^cache key data]
  (dosync (if (let [limit (:limit cache)]
                (and
                  (> limit 0)
                  (<= limit (count @(:data cache)))))
            (alter (:data cache) #(dissoc % (first (keys %)))))
          (alter (:data cache) assoc key data)
          (let [handler (:handler cache)]
            (when (fn? handler) (handler key data)))))
