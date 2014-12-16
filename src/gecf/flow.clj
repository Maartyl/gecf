(ns gecf.flow
  (:use gecf.graph))

(defrecord Gdata [height excess])

(def empty-gdata (Gdata. 0 0))

(defn- push-relabel-init [g start end]
  (let [stack (succs g start) ;; stack to use to identify nodes with excess, instead of searching
        ivs (map #(vector %1 (Gdata. 0 (weight g start %1))) stack) ;; {node gdata}, init excess with capacity: weight of first successors
        gm (into {} ivs) ;; create map of seq<[n gd]>; Gdata Map

        ;; all capacities set to 0; probably useless...
        resg {};(zipmap (concat (edges g) (edges (transpose g))) (repeat 0)) ;;residual capacities {[n1 n2] capacity} ; preflow
        fm (reduce #(assoc %1 ;; set f(s,succs*) = c(s,succs*) and f(succs*,s) to the negation
                        [start %2] (weight g start %2)
                        [%2 start] (- (weight g start %2))) resg stack)

        ]
    ;; set height of start to number of nodes; (unless graph is counted, that is O(n))
    [stack (assoc gm start (count (nodes g) )) fm]))

;;fg represents map of flows: {[n1 n2] flow}
(defn- flow     [fm n1 n2] (fm [n1 n2] 0)) ;; if not present: is 0 :: I may never ask for invalid flow
(defn- flow-set [fm n1 n2 c] (assoc fm [n1 n2] c))
(defn- flow-add [fm n1 n2 c] (let [fc (flow fm n1 n2)] (assoc fm [n1 n2] (+ fc c) [n2 n1] (- fc c)) )) ;;also substracts from opposite direction


(defn- height [gm n] (get-in gm [n :height] 0)) ;; or 0
(defn- height-inc
  ([gm n] (update-in gm [n :height] inc)) ;;for starters: simplest preflow-relabel: just lifts by 1; not to be used after first tests
  ([gm n c] (update-in gm [n :height] #(+ % c))))

(defn- excess [gm n] (get-in gm [n :excess] 0))
(defn- excess-set [gm n c] (assoc-in gm [n :excess] c))
(defn- excess-move [gm from to c] (-> gm (excess-set to c) (excess-set from (- c))))





;if (:e u) > 0 && uRv>0 && (:h u) == (inc (:h v))
;; v is under u and I push what what excess I can from u to v
;; already assumes it can happen
(defn- push [gm fm u v] (let [d (min (excess gm u) (flow fm u v))]
                          [(excess-move gm u v d)
                           (flow-add fm u v d)]))



(defn push-relabel
  [g start end]


  )
