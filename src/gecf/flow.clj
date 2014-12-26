(ns gecf.flow
  (:use gecf.graph))

;(defrecord Gdata [height excess])

;(defmacro queue [elems] `(conj (clojure.lang.PersistentQueue/EMPTY) ~@elems))

;(def empty-gdata (Gdata. 0 0))

(defn- gdata [h e] {:height h :excess e})

(defn- push-relabel-init [g start end]
  (let [active (succs g start) ;; stack to use to identify nodes with excess, instead of searching
        node-count (count (nodes g))
        ivs (map #(vector %1 (gdata 0 (- (weight g start %1)))) (preds g start))
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
        ;qa (queue active)
        qa (apply conj (clojure.lang.PersistentQueue/EMPTY) active)
        ]
    ; queue of active nodes; goldberg data map; flow function map; maximal height
    [qa gm fm maxh]))

;;fg represents map of flows: {[n1 n2] flow}
(defn- flow     [fm n1 n2] (fm [n1 n2] 0)) ;; if not present: is 0 :: I may never ask for invalid edge
(defn- flow-add [fm n1 n2 c] (update-in ;;symetric
                              (update-in fm [[n1 n2]] #(+ (or % 0) c))
                              [[n2 n1]] #(- (or % 0) c)) ) ;;also substracts from opposite direction


(defn- height     [gm n] (get-in gm [n :height] 0))
;(defn height-inc [gm n c] (update-in gm [n :height] #(+ (or % 0) c)))
(defn- height-set [gm n c] (assoc-in gm [n :height] c))

(defn- excess [gm n] (get-in gm [n :excess] 0))
(defn- excess-add [gm n c] (update-in gm [n :excess] #(+ (or % 0) c)))
(defn- excess-move [gm from to c] (-> gm (excess-add to c) (excess-add from (- c))))

(defn- residual [g fm u v] (- (weight g u v) (flow fm u v)))


;if (:e u) > 0 && uRv>0 && (:h u) == (inc (:h v))
;; v is under u and I push what what excess I can from u to v
;; already assumes it can happen
;; only adds v to actives: 'a ;; if excess moved
(defn- push [g start end gm fm a u v]
  (let [ruv (residual g fm u v)
        d (min (excess gm u) ruv)]
    (prn u v '% (excess gm u) ruv '% d (height gm u) (inc (height gm v)) '% (seq a) gm fm)
    ;; (if (and (pos? ruv) (= (height gm u) (inc (height gm v)))))
    (if (or #_ (= v start) (= v end)) ;; don't increment e(end) / or make active
      [(excess-move gm u v d) ;; (excess-add gm u (- d)) ;;only remove excess
       (flow-add fm u v d)
       a]
      [(excess-move gm u v d)
       (flow-add fm u v d)
       (if (zero? (excess gm v)) (conj a v) a)]  ;; add v to active nodes, if it had no excess until now
      )))

(defn- relabel [g gm fm maxh cur neighbours]
  (let [curh (height gm cur)
        _ (prn :relabel curh (zipmap neighbours (map (fn [%] [(residual g fm cur %) 'r:h (height gm %)]) neighbours)) )
        res (->> neighbours (filter #(pos? (residual g fm cur %))) (map #(height gm %)) (filter #(>= % curh)) (reduce min maxh) inc (height-set gm cur))
        _ (prn :res res)
        ]
    res))

;;(defn neighbours [g n] (concat (succs g n) (preds g n)))
(defn- neighbours [g n] (mapcat #(% g n) [succs preds]))

(defn- transferable [g fm gm cur neighbours]
  (filter #(and (pos? (residual g fm cur %1)) (= (height gm cur) (inc (height gm %1)))) neighbours))

(def STOPval (atom 100)) ;; stop infinite loops... (canceling execution doesn't work for this in LightTable, for some reason)
(defn- STOP [] (prn :stop @STOPval ) (neg? (swap! STOPval dec)) )

(defn push-relabel "Computes the maximal flow in graph 'g."
  [g start end]
  (let [[active gm fm maxh] (push-relabel-init g start end)]
    (reset! STOPval (* 3 maxh)) ;;DEBUG; can't be more then ~2V
    (loop [active active gm gm fm fm] ;; init loop with values from init
      (if (STOP) [:INFINITE maxh 'a (seq active) 'gm gm 'fm fm]
        (if (empty? active) fm ;; no more active nodes: return function of flow (flow-map)
          (let [cur (peek active)
                as (pop active)             ;;actives, that are not 'cur : used to compute* the rest (*adds to 'as)
                curh (height gm cur)]
            (prn :loop cur
                 (gm cur)
                 '%
                 (seq active)
                 gm
                 fm)
            (if (or (< maxh (height gm cur))) (recur as gm fm) ;; ignore end/start/too-high node
              (let [nghs (neighbours g cur) ;; nodes around ;;! potential growth
                    ts (transferable g fm gm cur nghs)
                    _ (prn :ts ts)
                    gm (if (empty? ts) (relabel g gm fm maxh cur nghs) gm)  ;; lift if nothing can be applied
                    ts (if (empty? ts) (transferable g fm gm cur nghs) ts)  ;; recompute transferable if relabeled ::lazy
                    _ (prn :ts ts)
                    [gm fm active] (reduce (fn [[gm fm a :as acc] n]
                                             (if (and (pos? (excess gm cur)))
                                               (push g start end  gm fm a  cur n) ;; move excess if possible
                                               (reduced acc)                      ;; nothing left to push
                                               )) [gm fm as] ts)]
                (if (and (pos? (excess gm cur)) ) ;; lift? : I tried push to all neighbours : still has excess...
                  (recur (conj active cur) (relabel g gm fm maxh cur nghs) fm) ;; try again in next pass
                  (recur active gm fm))                                         ;; active is processed in reduce; made of 'as
                ))))))))



(defn flow-size "Computes the |f| of flow f." [g fm start]
  (+ (apply + (map #(fm [start %]) (succs g start)))
     (apply + (map #(fm [% start]) (preds g start)))))












