(defproject gecf "0.1.1-SNAPSHOT"
  :description "computes k-edge-conectedness of a graph computing flow using preflow-relabel"
  :main gecf.core
  :aot :all
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :global-vars {*warn-on-reflection* true
                *assert* false} ; pre and post conditions
  :jvm-opts ["-Xmx1g"]

  )
