(require ['clojure.string :as 'str])

(defn mapkv
  "idiom to map to all kvs of a map"
  [f m]
  (into (empty m) (for [[k v] m] [(f k) (f v)])))


(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    (#(str/split % #"\n\n"))))

(def template
  (first input))

(def pairs
  (->>
    input
    second
    str/split-lines
    (map #(str/split % #" -> "))
    (#(into {} %))
    (#(mapkv seq %))
    ))

;rediiit
(def pairs-2
  (->>
    input
    second
    (re-seq #"([A-Z]+) -> ([A-Z])")
    (map (fn [[_ [a b] [c]]] {(list a b) [(list a c) (list c b)]}))
    (into {})
    ))


(defn new-pair
  [mapping p]
  (list (first p) (first (mapping p))))

(defn new-pair2
  [mapping p]
  (merge-with + (frequencies (new-pair mapping p))))

(defn step
  [in mapping]
  (str (str/join (mapcat #(str/join (new-pair mapping %)) (partition 2 1 in))) (last in)))

(defn step2
  [in mapping l]
  (apply merge-with + (cons l (mapcat (fn [[k count]] (map #(hash-map % count) (mapping k))) in))))

;part1 slow naive and simple
(loop
  [i template
   s 0]
  (if (= 10 s)
    (let [freqs (frequencies i)
          sorted-vals (sort (vals freqs))
          most-freq (last sorted-vals)
          least-freq (first sorted-vals)]
      (println freqs (- most-freq least-freq)))
    (recur (step i pairs) (inc s))))

;part2 counting
(def starting-map
  (let [ps (partition 2 1 template)]
    (frequencies ps)))

(def l
  (hash-map (list (last template)) 1))

(loop
  [i starting-map
   s 0]
  (if (= 40 s)
    (let [freqs-per-letter (apply merge-with + (mapcat (fn [[pair amount]] (vector {(first pair) amount})) i))
          max-freq (apply max (vals freqs-per-letter))
          min-freq (apply min (vals freqs-per-letter))
          ]
      (println (- max-freq min-freq)))
    (recur (step2 i pairs-2 l) (inc s))))


