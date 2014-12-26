(ns gecf.graph)

(defprotocol Graph
  (nodes [g] "seq of all nodes of a graph")
  (edges [g] "seq of all edges '([n1 n2]...)")
  (succs [g node] "successor nodes of a graph")
  (has-node? [g node])
  (has-edge? [g n1 n2] "is edge from n1 to n2 present?")
  (out-degree [g node] "the degree of outcoming edges"))

(defprotocol CountedGraph
  (count-nodes [g] "the count of all nodes, |V|")
  (count-edges [g] "the count of all edges, |E|"))

(defprotocol Digraph "represents directed graph with Graph"
  (preds [g node] "predecessors of given node")
  (in-degree [g node])
  (transpose [g] "returns graph with inverted edges"))

;;(defprotocol DataGraph
;;  (data [g node] "retrrieve the data associated with given node"))

(defprotocol WeightedGraph
  (weight [g n1 n2] "weight associated with given edge")
  (weighted-edges [g] "all edges, form [n1 n2 weight]"))



(defrecord AdjacentGraph [scs prs] ; scs{node #{succ succ}}; prs like scs but preds
  Graph
  (nodes [g] (keys scs))
  (edges [g] (for [[n1 n2s] scs
                   n2       n2s]
               [n1 n2]))
  (succs [g node] (scs node))
  (has-node? [g node] (contains? scs node))
  (has-edge? [g n1 n2] (contains? (scs n1) n2))
  (out-degree [g node] (count (scs node)))
  Digraph
  (preds [g node] (prs node))
  (in-degree [g node] (count (prs node)))
  (transpose [g] (assoc g :scs prs :prs scs))
 ;; DataGraph
 ;; (data [g node] (nds node))
  WeightedGraph
  (weight [g n1 n2] (if (has-edge? g n1 n2) 1 0))
  (weighted-edges [g] (map (fn [[n1 n2]] [n1 n2 (weight g n1 n2)]) (edges g)))
  )


(defn simple-digraph
  "given elems are assumed to by nodes, only vectors to be edges
  an edge can introduce a node
    (simple-digraph 4 5 6 [2 5])
  "

  [& elems] (let [or-empty-set (fnil identity #{})
                  [scs prs] (reduce (fn [[scs prs] e] ;;add each element (either vector: both, both directions) or just assure it's present
                                      (if (vector? e)
                                        (let [[n1 n2] e
                                              sv (-> n1 (scs #{}) (conj n2))  ;;get existing or empty; then add and push back
                                              pv (-> n2 (prs #{}) (conj n1))]
                                          [(assoc scs n1 sv n2 (scs n2 #{}))
                                           (assoc prs n2 pv n1 (prs n1 #{}))])
                                        (mapv #(update-in %1 [e] or-empty-set) [scs prs]) ; either the same, or add default: #{}
                                        )) [{} {}] elems)]
              (AdjacentGraph. scs prs)))


