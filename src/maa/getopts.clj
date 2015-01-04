(ns maa.getopts)



(defn or-alias [opts tag] (get (:aliases opts) tag tag))
(defn short-expand [opts tag] (get (:shorts opts) tag tag))

(defn parse
  " parses args list into map: -a lol -> {:opts {\\a true} :rest (\"lol\")}
  all options are optional:
  opts: {:has-data #{\"a\"}       ;; a: ; (matched against most processed 'alias')
         :aliases {\"lol\" :lol}  ;;not recursive
         :shorts {\\a \"auto\"}    ;; -a -> --auto :: means the same
        }
  "
  [opts args]
  (loop [args args ac {}] ;arg-container : {:opts {} :rest []}
    (if (empty? args) ac
      (let [^String cur (first args)]
        (cond
          (= "--" cur)           (assoc ac :rest (rest args))

          (.startsWith cur "--") (let [tag (.substring cur 2)
                                       al (or-alias opts tag)
                                       has-data (get (:has-data opts) al)]
                                   (recur (nthrest args (if has-data 2 1))
                                          (assoc-in ac [:opts al]
                                                    (if has-data (second args) true))))

         (.startsWith cur "-")   (recur (concat (map #(str "--" (short-expand opts %)) (rest cur))
                                                (rest args)) ac)

         :else (assoc ac :rest args)
         )))))
