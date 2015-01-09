(ns gecf.core
  (:use gecf.graph)
  (:use gecf.flow)
  (:use gecf.connected)

  (:use clojure.pprint)

  (:require [maa.getopts :as opts])
  (:require [clojure.edn :as edn])

  (:gen-class))

(defmacro get-version [] ;; known at compile time from Leiningen
  (System/getProperty "gecf.version"))


#_ "
basic code structure:

load data as some graph representation
 - possibly allow implicit edges, for I will need quite dense graphs to make sense of computing k-conectedness....
  - possible: thanks to Graph abstraction: there is not yet a way to define graph as input, but can be added quite easily
 - get all vertices but 1 and compute max-flow from that 1 to all others in parallel
  - return seq of all max-flows
  - choose minimal, to find _k
  - compute maximal cut from that flow
  -- use all maximal flows, to find all minimal cuts? - why? thrown away anyway...

 graph representation:
  - 2 parts: static and dynamic:
   - static shared across threads (edges)
   - dynmic is specialized <- outside 'graph' representation

representation options:
 - adjacency map {:A [:B :C], :B [:C] ...}
  - two of them to represent: edges and ''return edges''...

  normal graph, then:
  [nodes succs preds], where nodes is a map to node data :: not necessary : could be extern: simpler, more generic

edge representation:
 - [n1 n2]
 - [n1 n2 weight] <- {[n1 n2] weight}
"


(defn rand-graph
  "graph with random subset of edges : depends on density(0-1)
   - if size is collection instead: uses elements as nodes of graph"
  ([size density] ;; density:
   (apply simple-bigraph
          (let [nds (cond
                     (coll? size)   size
                     (number? size) (range size)
                     (symbol? size) (seq (str size))
                     :else          (seq size))]
            (for [x nds y nds :when (not= x y) :when (>= density (rand))] [x y]))))
  ([size] (rand-graph size 0.6)))

(defn full-graph [size] (rand-graph size 1))


(defn read-all
  ([] (read-all *in*))
  ([input]
   (let [eof (Object.)]
     (take-while #(not= % eof) (repeatedly #(edn/read {:eof eof} input))))))


(defn read-pairs [] (read-all))
(defn read-simple [] (map vec (partition 2 (read-all))))


(defn- flow-remove-0 [fm] (apply dissoc fm (for [[k v] fm :when (= v 0)] k))) ;;remove edges without any flow
(defn- flow-only-pos [fm] (apply dissoc fm (for [[k v] fm :when (<= v 0)] k))) ;;only positive flow

(defn compute-print [print-cut? g]
  (let [[size [start end] f] (min-max-flow g)]
    (if (neg? size) ;;empty graph
      (println size)
      (if print-cut?
        (let [cut (min-cut g f start)]
          (case print-cut?
            :all (pprint {:size size :cut cut :start start :end end :flow (flow-only-pos f)})
            :edn (pprint {:size size :cut cut})
            (do (println size) (pprint cut))))
        (println size)))))

(defn prn-help []
  (println "Gecf : compute k-edge-conectedness of given graph

  --bidirectional  -b    consider input to be an undirected graph (default)
  --directed       -d    consider input to be a directed graph

  --pairs          -p    input consists of nodes grouped to pairs: edges
                         // this allows defining vertices without any edges
                         // vertices must not be edn vectors
                         like: [1 2] [2 3] [3 1]         -> 2
                         like: 1 2 3 4 [1 2] [2 3] [3 1] -> 0

  --simple         -s    input is flat, 2 consecutive vertices are considered to be an edge (default)
                         like: 1 2 2 3 3 1

  --cut            -c    print minimal cut after it's size ;-> 2 ([1 2] [1 3])
  --cut-edn        -C    print minimal cut and size in edn map ;-> {:size 2 :cut ([1 2] [1 3])}
  --all            -A    print all in edn format ;-> {:size 2 :cut ([1 2] [1 3]) :start 1 :end 3 :flow ... }

  --full N         -f    use a full graph of N vertices (for testing etc.)
  --random N       -r    use an undirected graph of N vertices with
                         a random subset of edges (for testing etc.)

  --help           -h    show this help
  --version              show version of Gecf

What can be used as vertex identifier? - any edn structure/value. (You might prefer values that are fast to hash and compare.)
Stdin is assumed to be in edn format.

Return value -1 means the given graph was empty.
"))

(defn args-dispatch [args]
  (let [ac (opts/parse {:aliases{"bidirectional" :undig
                                 "directed" :digraph

                                 ;;ways of building graph:
                                 "pairs" :paired  ;; [x1 y1] [x2 y2]
                                 "simple" :simple ;; x1 y1 x2 y2
                                 ;"weighted" :weighted  ;; makes little sense here, but interesting [n1 n2 w]
                                 "implicit" :implicit

                                 "cut" :cut  ;; compute also minimal cut?
                                 "cut-edn" :cut-edn
                                 "all" :cut-all

                                 "full" :full ;; full, random graph: for testing etc.
                                 "random" :random ;; full, random: takes num of vertices as argument
                                 "help" :help
                                 "version" :version}
                        :shorts{\b "bidirectional"
                                \d "directed"
                                \p "pairs"
                                \s "simple"
                                \w "weighted"
                                \i "implicit"
                                \c "cut"
                                \C "cut-edn"
                                \A "all"
                                \f "full"
                                \r "random"
                                \h "help"}
                        :has-data #{:full :random}
                        } args)
        opt #(get-in ac [:opts %])

        graph-builder simple-bigraph
        graph-builder (if (opt :digraph) simple-digraph graph-builder)
        graph-builder (if (opt :undig) simple-bigraph graph-builder)   ;;prefered if both specified

        graph-reader read-simple
        graph-reader (if (opt :paired) read-pairs graph-reader)
        graph-reader (if (opt :simple) read-simple graph-reader)       ;;prefered if both specified

        print-cut? (opt :cut)
        print-cut? (if (opt :cut-edn) :edn print-cut?)
        print-cut? (if (opt :cut-all) :all print-cut?)]
    (cond
     (opt :version) (println (get-version))
     (opt :help) (prn-help)
     (opt :full) (->> (read-string (opt :full)) full-graph (compute-print print-cut?))
     (opt :random) (->> (read-string (opt :random)) rand-graph (compute-print print-cut?))
     :else (->> (graph-reader) (apply graph-builder) (compute-print print-cut?)) )))

(defn -main [& args]
  (args-dispatch args)

  (shutdown-agents)) ;;kill thread-pool

;(apply -main *command-line-args*)



