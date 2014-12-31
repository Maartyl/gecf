(ns gecf.connected
  (:use gecf.graph)
  (:use gecf.flow))


(defn max-flows [g]
  (let [[end & nds] (nodes g)]
      (map (fn [start]
              (let [f (push-relabel g start end)]
                (prn :f g [start end] f)
                [(flow-size g f start) [start end] f])) nds)))

(defn min-max-flow [g]
  (apply min-key first (max-flows g)))
