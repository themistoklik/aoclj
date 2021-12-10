(require ['clojure.string :as 'str])

(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    (#(str/split % #"\n\n"))))

(defn transpose
  [l]
  (apply map list l))

(defn parse-board
  [b]
  (->>
    b
    str/trim
    (#(str/split % #"\n"))
    (map #(str/split % #" "))
    (map #(filter not-empty %))
    (map #(vec %))))

(def numbers
  (map #(Integer/parseInt %) (str/split (first input) #",")))

(def boards
  (map parse-board (rest input)))

(def boardst
  (map transpose boards))

(defn set-rows
  [b]
  (map #(set %) b))

(def boards-set
  (map set-rows boards))

(def boardst-set
  (map set-rows boardst))

(defn mark
  [b n]
  (map #(disj % (str n)) b))

(defn won
  [b]
  (if (some true? (map empty? b))
    (->>
      b
      (reduce concat)
      (map #(Integer/parseInt %))
      (reduce + 0))
    0))


(defn drop-won
  [bs]
  (filter #(every? false? (map empty? %)) bs))
(loop
  [bs boards-set
   bst boardst-set
   nums numbers
   winning-num 0]

  (let
    [checkbs (map won bs)
     checkbst (map won bst)]
    (if (or (some true? (map #(> % 0) checkbst)) (some true? (map #(> % 0) checkbs)))
      (println (* winning-num (first (filter #(> % 0) checkbs))))
      (recur (map #(mark % (first nums)) bs) (map #(mark % (first nums)) bst) (rest nums) (first nums)))))


(loop
  [bs boards-set
   bst boardst-set
   nums numbers
   winning-num 0]
  (println winning-num (map won bs) (map won bst))
  (if (or (= 0 (count bs)) (= 0 (count bst)))
    (println 1)
    (recur (map #(mark % (first nums)) (drop-won bs)) (map #(mark % (first nums)) (drop-won bst)) (rest nums) (first nums))))