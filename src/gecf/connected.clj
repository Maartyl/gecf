(ns gecf.connected
  (:use gecf.graph)
  (:use gecf.flow))


(defn max-flows [g]
  (let [[end & nds] (nodes g)]
      (map (fn [start]
              (let [[fs f] (push-relabel g start end)]
                (prn :f [start end] f)
                [fs [start end] f])) nds)))

(defn max-flows-all [g]
  (for [start (nodes g)
        end   (nodes g)]
    (let [[fs f] (push-relabel g start end)]
      (prn :f [start end] f)
      [fs [start end] f])))

(defn min-max-flow [g]
  (apply min-key first (max-flows g)))


;; def: min-cut
