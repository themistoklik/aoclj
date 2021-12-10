(require ['clojure.string :as 'str]
         ['clojure.set :as 'set])

(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/split-lines
    (mapv #(apply list %))))

(def opposites
  {\[ \] \{ \} \( \) \< \>})

(def openers
  (set (keys opposites)))

(def closers
  (set (vals opposites)))

(def scores
  {\) 3 \] 57 \} 1197 \> 25137})

(def scores-pt2
  {\) 1 \] 2 \} 3 \> 4})

(defn score-incomplete-line
  [l]
  (reduce #(+ (* %1 5) %2) 0 (map scores-pt2 l)))

(defn take-mid
  "take mid from always odd size col"
  [c]
  (let [mid (quot (count c) 2)]
    (nth c mid)))

(defn validate-line
  [vs]
  (loop [vals vs
         expected []]
    (let [curr (first vals)
          closer? (contains? closers curr)
          opener? (contains? openers curr)
          expected-closer? (and closer? (= curr (first expected)))]
      (cond
        opener? (recur (rest vals) (cons (opposites curr) expected))
        expected-closer? (recur (rest vals) (rest expected))
        :else [curr expected]))))


; part1
(->>
  input
  (map validate-line)
  (filter #(not (nil? (first %))))
  (map first)
  (map scores)
  (reduce + 0)
  println)

; part2
(->>
  input
  (map validate-line)
  (filter #(nil? (first %)))
  (map second)
  (map score-incomplete-line)
  sort
  take-mid
  println)
