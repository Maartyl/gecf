(ns gecf.connected
  (:use gecf.graph)
  (:use gecf.flow))


(defn max-flows [g]
  (let [[end & nds] (nodes g)]
      (map (fn [start]
              (let [f (push-relabel g start end)]
                (prn :f [start end] g f)
                [(flow-size g f end) [start end] f])) nds)))

(defn min-max-flow [g]
  (apply min-key first (max-flows g)))


;; def: min-cut
