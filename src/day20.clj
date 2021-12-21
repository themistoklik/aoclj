(require ['clojure.string :as 'str])

(def inp
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    (#(str/split % #"\n\n"))))

(def algorithm
  (let [algorithm-s (str/replace (first inp) #"\n" "")]
    (->>
      (interleave (range (count algorithm-s)) algorithm-s)
      (partition 2)
      (map vec)
      (into {})
      )))

(def image
  (->>
    inp
    second
    str/split-lines
    (map vec)
    vec))

(defn neighbors
  ([xy]
   (neighbors [[-1 -1] [-1 0] [-1 1]
               [0 -1] [0 0] [0 1]
               [1 -1] [1 0] [1 1]]
              xy))
  ([deltas xy]
   (let [applied-deltas (map #(vec (map + xy %)) deltas)]
     applied-deltas)))

(defn get-bin
  [s]
  (Long/parseLong (apply str s) 2))

(defn get-num
  [image coord blink]
  (let [translate {\. 0 \# 1}
        rtranslate {0 \. 1 \#}]
    (->> (neighbors coord)
         (map #(get-in image % (rtranslate blink)))
         (map #(translate %))
         get-bin)))


(defn enhance
  [img algo i]
  (let [blink (if (= \. (second (first algo))) 0 (mod i 2))
        xs (count img)
        ys (count (first img))]
    (vec (for [x (range -1 (inc xs))]
           (vec (for [y (range -1 (inc ys))]
                  (algo (get-num img [x y] blink))))))))


;part 1 n == 2 part 2 n == 50
(loop [n 0
       img image]
  (if (= 50 n)
    (println (frequencies (mapcat identity img)))
    (recur (inc n) (enhance img algorithm n))))