(defproject mesh-network-clojure "0.1.0-SNAPSHOT"
  :description "Mesh-network-framework, Make UTXO Great Again for clojure implementation"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/algo.monads "0.1.6"]
                 [org.clojure/core.async "0.4.490"]
                 [commons-codec/commons-codec "1.12"]]
  :main ^:skip-aot mesh-network-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
