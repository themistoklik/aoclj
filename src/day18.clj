; Today i ended up needing hints and the code i studied was https://github.com/wevre/advent-of-code/blob/master/src/advent_of_code/2021/day_18.clj
; i learned about zippers which was really cool, and i decided to post this solution since i was
; trying to code this anyway. I abandoned my string manipulation one pretty early to parse this into a tree
; and then i found out about zippers which led me to that nice impl. I learned something today. I wish i quit the
; bad impl sooner and just studied zippers from the start.

;the code i ended up writing was so similar to wevre's that i thought what's the point let's RT this great piece of code


(:require ['clojure.zip :as 'zip]
  ['clojure.string :as 'str])

;; --- Day 18: Snailfish ---
;; https://adventofcode.com/2021/day/18

(defn parse-input [s]
  (->> s str/split-lines (map read-string)))

;; Finding nodes of interest in the zipper.

(defn regular-number? [loc]
  (number? (zip/node loc)))

(defn regular-pair? [loc]
  (let [x (zip/node loc)]
    (and (vector? x) (number? (first x)) (number? (second x)))))

(defn explodable? [loc]
  (and (regular-pair? loc) (<= 4 (count (zip/path loc)))))

(defn splitable? [loc]
  (let [x (zip/node loc)] (and (number? x) (<= 10 x))))

(defn find-loc
  ([pred loc] (find-loc pred loc zip/next))
  ([pred loc move]
   (loop [z (move loc)]
     (when-not (or (nil? z) (zip/end? z))
       (if (pred z) z (recur (move z)))))))

;; Reducing actions.

(defn incr-neighbor
  "Increase a regular number neighbor of `loc` by amount `v`, searching in
   direction `dir` (must be `:next` or `:prev`). After the edit, find and return
   the loc of original node (by searching in the opposite direction). If no
   neighbor found, return original `loc`."
  [loc dir v]
  (let [neighbor (partial find-loc regular-number?)
        [f f'] (if (= :next dir) [zip/next zip/prev] [zip/prev zip/next])]
    (if-let [z (neighbor loc f)]
      (-> z (zip/edit + v) (neighbor f'))
      loc)))

(defn explode [loc]
  (let [[r l] (zip/node loc)]
    (-> (zip/replace loc 0)
        (incr-neighbor :prev r)
        (incr-neighbor :next l)
        zip/root)))

(defn split [loc]
  (let [v (/ (zip/node loc) 2)
        v' [(int (Math/floor v)) (int (Math/ceil v))]]
    (-> loc (zip/replace v') zip/root)))

;; Adding and reducing snailfish numbers.

;; Wrap snailfish numbers in a map that includes a flag indicating if we took
;; any edit actions. That is the signal that explosions and splits are all done.

(defn apply-one-action [{sfn :sfn}]
  (let [mark-edited (fn [f] (fn [loc] {:edit? true :sfn (f loc)}))]
    (condp find-loc (zip/vector-zip sfn)
      explodable? :>> (mark-edited explode)
      splitable? :>> (mark-edited split)
      {:edit? false :sfn sfn})))

(defn add-sfn [a b]
  (->> {:edit? true :sfn [a b]}
       (iterate apply-one-action)
       (drop-while :edit?)
       first
       :sfn))

(defn magnitude [sfn]
  (if (number? sfn) sfn (reduce + (map * (map magnitude sfn) [3 2]))))

;; part 1
(time
  (->> (slurp "/Users/themis/IdeaProjects/aoclj/src/src/input.txt")
       parse-input (reduce add-sfn) magnitude))             ;=>4137

;; part 2
(time
  (let [sfns (parse-input (slurp "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"))]
    (->> (for [a sfns b sfns :when (not= a b)] (add-sfn a b))
         (map magnitude)
         (reduce max))))


