(require ['clojure.string :as 'str])

;(defn mapkv
;  "idiom to map to all kvs of a map"
;  [f m]
;  (into (empty m) (for [[k v] m] [(f k) (f v)])))

(defn fmap
  "idiom to map to all vals of a map"
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn
  to-list-of-ints
  [s]
  (mapv #(Character/digit % 10) s))

(defn calc-first-col
  [input]
  (reduce (fn [in [x y]] (assoc-in in [x y] (+ (get-in in [(dec x) y]) (get-in in [x y])))) input (for [x (range 1 (count input)) y [0]] [x y])))

(defn calc-first-row
  [input]
  (reduce (fn [in [x y]] (assoc-in in [x y] (+ (get-in in [x (dec y)]) (get-in in [x y])))) input (for [x [0] y (range 1 (count input))] [x y])))

(def in
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/split-lines
    (mapv #(apply vector %))
    (mapv to-list-of-ints)))

(defn inc-and-wrap-9
  [n]
  (if (= n 8) 9 (rem (inc n) 9)))

(defn expand-x1
  [row]
  (mapv #(inc-and-wrap-9 %) row))

(def input
  (->>
    in
    calc-first-col
    calc-first-row
    ))

(def grid (into {}
                (for [x (range (count input)) y (range (count input))]
                  [[x y] (get-in input [x y])])))

(defn valid-coords
  [c grid]
  (contains? grid c))

(defn neighbors
  ([yx g]
   (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
              yx g))
  ([deltas yx g]
   (let [applied-deltas (map #(vec (map + yx %)) deltas)]
     (filter #(valid-coords % g) applied-deltas))))


(def iscores
  (assoc-in (vec (repeat (count input) (vec (repeat (count input) 999)))) [0 0] 0))

(defn p1-costs
  [scores input]
  (reduce (fn
            [in [x y]]
            (assoc-in in [x y]
                      (let [current-val (get-in input [x y])
                            neighs (vector (get-in scores [x (inc y)]) (get-in scores [x (dec y)]) (get-in scores [(inc x) y]) (get-in scores [(dec x) y]))
                            valid-neighs (filter identity neighs)
                            min-neighbor (apply min valid-neighs)]
                        (+ current-val min-neighbor))))
          scores
          (for [x (range 1 (count scores)) y (range 1 (count scores))] [x y])))
;part1
(loop [in input
       curr (p1-costs in in)
       next [[]]]
  (if (= (last (last next)) (last (last curr)))
    (println (- (last (last curr)) (first (first curr))))
    (recur in (p1-costs curr in) curr)))


(defn add-wrap [a b]
  (let [sum (+ a b)]
    (if (> sum 9)
      (- sum 9)
      sum)))

(defn expand-y [xdata]
  (vec (for [my (range 0 5)
             row xdata]
         (vec (for [col row]
                (add-wrap col my))))))

(defn expand-x [data]
  (vec (for [row data]
         (vec (for [mx (range 0 5)
                    col row]
                (add-wrap col mx))))))


;part2
(loop [in (->> in expand-x expand-y calc-first-row calc-first-col)
       curr (p1-costs in in)
       next [[]]]
  (if (= (last (last next)) (last (last curr)))
    (println (- (last (last curr)) (first (first curr))))
    (recur in (p1-costs curr in) curr)))