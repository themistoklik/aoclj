(require ['clojure.string :as 'str])

(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    (#(str/split % #"\n\n"))))

;x increases to the right y downwards
(def coords
  (->>
    input
    first
    (#(str/split % #"\n"))
    (map #(str/split % #","))
    (map #(map read-string %))
    (#(zipmap % (repeat (count %) "#")))))

(def instructions
  (->>
    input
    second
    str/split-lines
    (map #(re-find (re-pattern "[x|y]=\\d+") %))
    (map #(str/split % #"="))
    (map #(vector (first %) (read-string (second %))))))


(defn fold-up
  [grid y]
  (let [g (filter #(< y (second (first %))) grid)
        new-ys (mapcat #(vector (list (first (first %)) (- y (Math/abs (- y (second (first %)))))) "#") g)]
    (apply assoc (apply dissoc grid (keys g)) new-ys)))

(defn fold-left
  [grid x]
  (let [g (filter #(< x (first (first %))) grid)
        new-xs (mapcat #(vector (list (- x (Math/abs (- x (first (first %))))) (second (first %))) "#") g)]
    (apply assoc (apply dissoc grid (keys g)) new-xs)))


;part 1
(loop [ins [(first instructions)]
       g coords]
  (let
    [i (first ins)
     dir (first i)
     amount (second i)]
    (cond
      (empty? i) (println (count g))
      (= "y" dir) (recur (rest ins) (fold-up g amount))
      (= "x" dir) (recur (rest ins) (fold-left g amount))
      :else (println "wat"))))


; stolen from reddit much cleaner than my cacophony or reduce assoc-ins
(defn print-dots [dots]
  (let [keys (keys dots)
        xmax (apply max (map first keys))
        ymax (apply max (map second keys))]
    (doseq [y (range (inc ymax))]
      (doseq [x (range (inc xmax))]
        (print (if (get dots [x y]) "#" ".")))
      (println))))

;part 2 2 lazy to make a foo
(loop [ins instructions
       g coords]
  (let
    [i (first ins)
     dir (first i)
     amount (second i)]
    (cond
      (empty? i) (println (gridify g))
      (= "y" dir) (recur (rest ins) (fold-up g amount))
      (= "x" dir) (recur (rest ins) (fold-left g amount))
      :else (println "wat"))))