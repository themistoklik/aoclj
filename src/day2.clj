(require ['clojure.string :as 'str])


(def input
  (->>
    "input.txt"
    slurp
    str/split-lines
    (map #(str/split % #" "))))

(def depth
  {:up   #(- %)
   :down #(+ %)})

(def aim
  {:up   #(+ %)
   :down #(- %)})




(defn calc-depth
  [vs]
  (->>
    vs
    (map #((depth (keyword (first %))) (Integer/parseInt (second %))))
    (reduce + 0)))

(def vert
  (set ["forward"]))

(let [groups (group-by #(contains? vert (first %)) input)
      h (->>
          (groups true)
          (map #(Integer/parseInt (second %)))
          (reduce + 0))
      v (->>
          (groups false)
          calc-depth)]

  (println (* h v)))

;[horizontal vert aim]
(defn part-2
  [acc inp]
  (let [command (keyword (first inp))
        amount (Integer/parseInt (second inp))
        prevHorizontal (first acc)
        prevVertical (second acc)
        prevAim (last acc)]
    (condp = command
      :up [prevHorizontal prevVertical (- prevAim amount)]
      :down [prevHorizontal prevVertical (+ prevAim amount)]
      :forward [(+ prevHorizontal amount) (+ prevVertical (* prevAim amount)) prevAim]
      )))

(let [solve (reduce part-2 [0 0 0] input)]
  (println (* (first solve) (second solve)))
  )
