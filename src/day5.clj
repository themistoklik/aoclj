(require ['clojure.string :as 'str])


(defn keep-straight
  [c]
  (let [x1 (first (first c))
        y1 (second (first c))
        x2 (first (second c))
        y2 (second (second c))]
    (or (= x1 x2) (= y1 y2))))

(defn gen-pairs
  [e]
  (->>
    e
    (mapv #(str/split % #","))
    (mapv #(mapv read-string %))))

(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/split-lines
    (mapv #(str/split % #" -> "))
    (mapv gen-pairs)
    ))

(defn expand-into-range-no-diag
  [c]
  (let [x1 (first (first c))
        y1 (second (first c))
        x2 (first (second c))
        y2 (second (second c))
        sorted (sort c)]

    (if (= y1 y2)
      (for [n (range (first (first sorted)) (inc (first (second sorted))))] (vector n y1))
      (for [n (range (second (first sorted)) (inc (second (second sorted))))] (vector x1 n)))
    ))


(defn get-range
  [c1 c2]
  (if (> c1 c2)
    (vec (range c2 (inc c1)))
    (vec (reverse (range c1 (inc c2))))))

(defn expand-into-range-diag
  [c]
  (let [x1 (first (first c))
        y1 (second (first c))
        x2 (first (second c))
        y2 (second (second c))
        sorted (sort c)]

    (cond
      (= y1 y2) (for [n (range (first (first sorted)) (inc (first (second sorted))))] (vector n y1))
      (= x1 x2) (for [n (range (second (first sorted)) (inc (second (second sorted))))] (vector x1 n))
      :else (map vector (get-range x1 x2) (get-range y1 y2)))))

; part1
(def in1
  (->>
    input
    (filterv keep-straight)
    (mapv expand-into-range-no-diag)
    (reduce concat)
    frequencies
    vals
    frequencies
    ))

(println (reduce + 0 (vals (dissoc in1 1))))

;part 2
(def in2
  (->>
    input
    (mapv expand-into-range-diag)
    (reduce concat)
    frequencies
    vals
    frequencies
    ))

(println (reduce + 0 (vals (dissoc in2 1))))
