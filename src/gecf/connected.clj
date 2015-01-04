(ns gecf.connected
  (:use gecf.graph)
  (:use gecf.flow))


(defn max-flows [g]
  (let [[end & nds] (nodes g)]
      (pmap (fn [start]
              (let [[fs f] (push-relabel g start end)]
                [fs [start end] f])) nds)))

(defn max-flows-all [g]
  (for [start (nodes g)
        end   (nodes g)]
    (let [[fs f] (push-relabel g start end)]
      [fs [start end] f])))

(defn min-max-flow [g]
  (apply min-key first (or (max-flows g) [[0 :empty]])))


;; def: min-cut

#_"

DFS: stop on saturated, remeber them (it's cut)
 - nonsaturated: recursively repeat

 :saturated: residual == 0
  - pos -> not saturated
  - negative? ... flows more then possible... (or nonsatuared edge in opposite direction exists)
   - not saturated.
   - ... but should I continue with recursion? - going 'against'(!) the flow?

 -0: yield
 -pos: recur
 -neg: ignore

 problem: something is saturated, but not part of min cut: narrower later, this being part of it...
  - [s 1 5/5] [1 2 5/10] [s 2 2/3] [3 t 7/7] ;; 2/5 means: flow:2, capacity:5
  - this simple approach wouldn't work...
  - it would only find A cut, not min-cut

  !!!
  only dfs* and mark non-saturated edges: all (put to set) (*dfs: all nonsaturated, from source)
  then traverse set and get all edges that start in it and lead out :: that is cut

"


