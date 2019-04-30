(ns mesh-network-clojure.utils.cache)

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
