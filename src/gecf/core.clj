(ns gecf.core
  (:use gecf.graph)
  (:use gecf.flow)
  (:use gecf.connected)
  (:use clojure.pprint)
  (:require [maa.getopts :as opts] )
  (:require [clojure.edn :as edn])
  (:gen-class))

(def __gecf-version__ "0.0.1")

#_ "
basic code structure:

load data as some graph representation
 - possibly allow implicit edges, for I will need quite dense graphs to make sense of computing k-conectedness....
  - possible: thanks to Graph abstraction: there is no way to define graph as input, but can be added quite easily
 - get all vertices but 1 and compute max-flow from that 1 to all others
  - return seq of all max-flows
  - choose minimal, to find _k
  - compute maximal cut from that flow
  -- use all maximal flows, to find all minimal cuts?

 graph representation:
  - 2 parts: static and dynamic:
   - static shared across threads (edges)
   - dynmic is specialized <- outside 'graph' representation

  - what do I need:
   - forward edges
   - backward edges
   - NO edge capacity: I can use simpler max-flow using only boolean capacities (not necessarily represented by values)

   what operations do I need?
    - succ-s
    - pred-s ??

    - edges in residual graph

    - nodes: all nodes
    - edges: all edges
    - has-node? [g n]
    - has-edge? [g n1 n2]
    - out-degree

  - Digraph ...
    - predecessors
    - in-degree
    transpose (swap {in, out} maps)

  - weighted graph:
    - weight [n1 n2]

representation options:
 - adjacency map {:A [:B :C], :B [:C] ...}
  - two of them to represent: edges and ''return edges''...

  normal graph, then:
  [nodes succs preds], where nodes is a map to node data :: not necessary : could be extern: simpler

edge representation:
 - [n1 n2]
 - [n1 n2 weight] <- {[n1 n2] weight}
 = and possibly label that somehow...
"

(defn full-graph [size]
  (apply simple-bigraph (for [x (range size) y (range size) :when (< x y)] [x y])))


(defn read-all
  ([] (read-all *in*))
  ([input]
   (let [eof (Object.)] ;; possibly change to edn for safety... (but future? implicit graphs are unsafe anyway); different eof semantics
     (take-while #(not= % eof) (repeatedly #(read input false eof))))))


(defn read-pairs [] (read-all *in*))
(defn read-simple [] (map vec (partition 2 (read-all *in*))))


(defn compute-print [print-cut? g]
  (if print-cut?
    (prn "Can't compute minimal cut yet.")
    (-> g min-max-flow first prn)))

(defn prn-help []
  )

(defn args-dispatch [args]
  (let [ac (opts/parse {:aliases{"bidirectional" :undig
                                 "directed" :digraph

                                 ;;ways of building graph:
                                 "pairs" :paired  ;; [x1 y1] [x2 y2]
                                 "simple" :simple ;; x1 y1 x2 y2
                                 ;"weighted" :weighted  ;; makes little sense here, but interesting [n1 n2 w]
                                 "implicit" :implicit

                                 "cut" :cut  ;;unsupported ;; compute also minimal cut?
                                 "full" :full ;; full graph: for testing etc.
                                 "help" :help
                                 "version" :version
                                 }
                        :shorts{\b "bidirectional"
                                \d "directed"
                                \p "pairs"
                                \s "simple"
                                \w "weighted"
                                \i "implicit"
                                \c "cut"
                                \f "full"
                                \h "help"
                                }
                        :has-data #{:full}
                        } args)
        opt #(get-in ac [:opts %])

        graph-builder simple-bigraph
        graph-builder (if (opt :digraph) simple-digraph graph-builder)
        graph-builder (if (opt :undig) simple-bigraph graph-builder)

        graph-reader read-simple
        graph-reader (if (opt :paired) read-pairs graph-reader)
        graph-reader (if (opt :simple) read-simple graph-reader)

        print-cut? (get-in ac [:opts :cut])
        ]
    (cond
     (opt :version) (prn 'Gecf __gecf-version__)
     (opt :help) (prn "-b -d, -p -s ... some help")
     (opt :full) (->> (read-string (opt :full)) full-graph (compute-print print-cut?))
     :else (->> (graph-reader) (apply graph-builder) (compute-print print-cut?))
     )))

(defn -main [& args]
  (args-dispatch args)

  (shutdown-agents)) ;;kill thread-pool





;(apply -main *command-line-args*)



