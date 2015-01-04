(ns gecf.core
  (:use gecf.graph)
  (:use gecf.flow)
  (:use gecf.connected)
  (:use clojure.pprint)
  (:require [maa.getopts :as opts] )
  (:require [clojure.edn :as edn]))

(def __gecf-version__ "0.0.1")

#_ "
basic code structure:

load data as some graph representation
 - possibly allow implicit edges, for I will need quite dense graphs to make sense of computing k-conectedness....
 - get all vertices but 1 and compute max-flow from that 1 to all others
  - return seq of all max-flows
  - choose minimal, to find _k
  - compute maximal cut from that flow
  -- use all minimal flow, to find all maximal cuts?

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


(defn read-pairs [] (map vec (take-while #(not= % :MAA_EOF) (repeatedly (edn/read {:eof :MAA_EOF} *in*)))))
(defn read-simple [] (map vec (partition 2 (take-while #(not= % :MAA_EOF) (repeatedly (edn/read {:eof :MAA_EOF} *in*))))))


(defn compute-print [print-cut? g]
  (if print-cut?
    (prn "Can't compute minimal cut yet.")
    (-> g min-max-flow first prn))
  )

(defn args-dispatch [args]
  (let [ac (opts/parse {:aliases{"bidirectional" :undig
                                 "directed" :digraph

                                 ;;ways of building graph:
                                 "pairs" :paired  ;; [x1 y1] [x2 y2]
                                 "simple" :simple ;; x1 y1 x2 y2
                                 ;"weighted" :weighted  ;; makes little sense here, but interesting

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
                                \c "cut"
                                \f "full"
                                \h "help"
                                }
                        :has-data #{:full}
                        } args)
        graph-builder simple-bigraph
        graph-builder (if (get-in ac [:opts :digraph]) simple-digraph graph-builder)
        graph-builder (if (get-in ac [:opts :undig]) simple-digraph graph-builder)

        graph-reader read-simple
        graph-reader (if (get-in ac [:opts :paired]) simple-digraph graph-reader)

        print-cut? (get-in ac [:opts :cut])
        ]
    (cond
     (get-in ac [:opts :version]) (prn "Gecf " __gecf-version__)
     (get-in ac [:opts :help]) (prn "-b -d, -p -s ... some help")
     (get-in ac [:opts :full]) (->> (read-string (get-in ac [:opts :full])) inc full-graph (compute-print print-cut?))
     :else (->> (graph-reader) graph-builder (compute-print print-cut?))
     )


    ))

(defn -main [& args]
  (args-dispatch args)
;;   (pprint (doall (min-max-flow
;; ;;                   (simple-digraph
;; ;;                              ;[1 2] [3 4] [2 4] [1 3] [3 5] [4 5] [2 6] [5 6]
;; ;;                              [1 2] [2 1] [1 3] [3 1] [2 4] [4 2] [3 4] [4 3] [2 5] [5 2] [3 5] [5 3]
;; ;;                              )
;; ;;                             (simple-bigraph
;; ;;                              [1 2] [1 3] [2 4] [3 4] [2 5] [3 5])
;;                   (full-graph 20)
;;                             )))
  (shutdown-agents)) ;;kill thread-pool





;(apply -main *command-line-args*)



