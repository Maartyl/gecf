(ns gecf.flow
  (:use gecf.graph))

(defn- gdata [h e] {:height h :excess e}) ;; Goldberg data

(defn- push-relabel-init [g start end]
  (let [active (succs g start) ;; stack to use to identify nodes with excess, instead of searching
        node-count (count-nodes g)
        ;; saturate residual edges around start :excess
        ivs (map #(vector %1 (gdata 0 (- (weight g start %1)))) (preds g start)) ;; unsure whether I need this...
        ;; {node gdata}, init excess with capacity: weight of first successors
        ivs (concat ivs (map #(vector %1 (gdata 0 (weight g start %1))) active))
        gm (into {} ivs) ;; create map of seq<[n gd]>; Gdata Map
        gm (assoc gm start (gdata node-count (/ 1.0 0))) ;; h(start) <- N; e(s) <- infinity

        ;; all capacities set to 0; useless... :: assume 0 implicitly
        fm {};(zipmap (concat (edges g) (edges (transpose g))) (repeat 0)) ;;residual capacities {[n1 n2] capacity} ; preflow
        fm (reduce #(assoc %1 ;; set f(s,succs*) = c(s,succs*) and f(succs*,s) to the negation
                      [start %2] (weight g start %2)
                      [%2 start] (- (weight g start %2)) ;;symetric
                      ) fm active)
        maxh (dec (* 2 node-count)) ;;maximal height of a node (2N-1)
        ;qa: queue of active nodes
        qa (into (clojure.lang.PersistentQueue/EMPTY) (filter #(and (not= % start) (not= % end)) active))
        ]
    ; queue of active nodes; goldberg data map; flow function map; maximal height
    [qa gm fm maxh]))

;;fm represents map of flows: {[n1 n2] flow}
(defn- flow     [fm n1 n2] (fm [n1 n2] 0)) ;; if not present: is 0 :: I may never ask for invalid edge
(defn- flow-add [fm n1 n2 c] (update-in ;;symetric
                              (update-in fm [[n1 n2]] #(+ (or % 0) c))
                              [[n2 n1]] #(- (or % 0) c)) ) ;;also substracts from opposite direction


(defn- height     [gm n] (get-in gm [n :height] 0))
(defn- height-set [gm n c] (assoc-in gm [n :height] c))

(defn- excess [gm n] (get-in gm [n :excess] 0))
(defn- excess-add [gm n c] (update-in gm [n :excess] #(+ (or % 0) c)))
(defn- excess-move [gm from to c] (-> gm (excess-add to c) (excess-add from (- c))))

(defn residual [g fm u v] (- (weight g u v) (flow fm u v)))
(defn- residual-pos? [g fm u v] (pos? (residual g fm u v)))

(defn- activate "adds v to 'a if appropriate" [start end old-gm a v] ;; if it had excess: already present in queue
  (if (and (not= v start) (not= v end) (zero? (excess old-gm v))) (conj a v) a))

;; u -> v
;; already assumes it can happen :: (:e u) > 0 && uRv>0 && (:h u) == (inc (:h v))
;; adds to queue if becomes active ;; 'a
(defn- push [g start end fm gm a u v]
  (let [ruv (residual g fm u v)
        d (min (excess gm u) ruv)]
    ;(prn :push [u v] '% :e<u> (excess gm u) :res ruv '% :d d :h (height gm u) (inc (height gm v)) '% (seq a) gm fm)
    [(excess-move gm u v d)
     (flow-add fm u v d)
     (activate start end gm a v)]))

(defn neighbours [g n] (concat (succs g n) (preds g n)))

(defn- relabel [g gm fm maxh cur] ;; if edge not filled, get min of bigger heights (or maxh) |> inc |> set
  (let [nonsaturated? (partial residual-pos? g fm cur)
        h (partial height gm)
        gteq? (partial <= (height gm cur))]
    (->> (neighbours g cur) (filter nonsaturated?) (map h) (filter gteq?) (reduce min maxh) inc (height-set gm cur))))

(defn- can-push? "from 'u to 'v ?" [g fm gm u v]
  (and (residual-pos? g fm u v) (= (height gm u) (inc (height gm v)))))

(defn- try-push "can-push? ? push : dflt arg" [g start end fm gm a  u v dflt]
  (if (can-push? g fm gm u v) (push g start end fm gm a u v) dflt))

;(defn- transferable [g fm gm cur neighbours] (filter #(can-push? g fm gm cur %) neighbours))

;;(def STOPval (atom 100)) ;; stop infinite loops... (canceling execution doesn't work for this in LightTable, for some reason)
;;(defn- STOP [] (prn :stop @STOPval ) (neg? (swap! STOPval dec)) )

(defn push-relabel "Computes the maximal flow in graph 'g."
  [g start end]
  (let [[active gm fm maxh] (push-relabel-init g start end)]
;;     (reset! STOPval (* 2 maxh maxh))        ;; DEBUG; can't be more then ~2V^2
    (loop [active active gm gm fm fm]          ;; init loop with values from init
;;       (if (STOP) [:INFINITE maxh :a (seq active) :gm gm :fm fm])
      (if (empty? active) [(excess gm end) fm] ;; no more active nodes: return function of flow (flow-map) and size of flow (excess in 't)
        (let [cur (peek active)
              as (pop active)                  ;;actives, that are not 'cur : used to compute* the rest (*adds to 'as)
              curh (height gm cur)]
          ;(prn :loop cur (gm cur) '% (seq active) gm fm)
          (if (< maxh curh) (recur as gm fm) ;; ignore too-high nodes
            (let [[gm fm active] (reduce (fn [[gm fm a :as acc] n]
                                           (if (pos? (excess gm cur))
                                             (try-push g start end  fm gm a  cur n  acc)      ;; move excess if possible
                                             (reduced acc))) [gm fm as] (neighbours g cur))]  ;; nothing left to push
              (if (pos? (excess gm cur))  ;; lift? : I tried push to all neighbours : still has excess...
                (recur (conj active cur) (relabel g gm fm maxh cur) fm)                       ;; try again in next pass
                (recur active gm fm)))))))))                                                  ;; active is processed in reduce; made of 'as





