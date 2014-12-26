(ns gecf.core
  (:use gecf.graph)
  (:use gecf.flow))


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
   - static shared across threads
   - dynmic is
  - all is immutable, could be one, but, would it be fast enough?

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
    transpose (swap in, out maps)

  - weighted graph:
    - weight [n1 n2]

representation options:
 - adjacency map {:A [:B :C], :B [:C] ...}
  - two of them to represent: edges and ''return edges''...

  normal graph, then:
  [nodes succs preds], where nodes is a map to node data

edge representation:
 - [n1 n2]
 - [n1 n2 weight] <- {[n1 n2] weight}
 = and possibly label that somehow...



"


;(push-relabel (simple-digraph [1 2] [3 4] [2 4] [1 3] [3 5] [4 5] [2 6] [5 6]) 1 6)

(push-relabel (simple-digraph [1 2] [2 3] [3 1]) 3 2)

;; (def g (simple-digraph [1 2] [3 4] [2 4] [1 3]))

;; (push-relabel-init g 1 3)
;; (let [[a gm fm maxh] (push-relabel-init g 1 3)]
;;   (def as a)
;;   (def gm gm)
;;   (def fm fm)
;;   (def maxh maxh))
;; gm
;; gm
;; (excess-move gm 3 4 5 )






