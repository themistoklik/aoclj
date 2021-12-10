(require ['clojure.string :as 'str])

(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    (#(str/split % #","))
    (mapv read-string)))

(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))


(defn avg [ns]
  (let [cnt (count ns)
        nssum (reduce + 0 ns)]
    (/ nssum cnt)))

(def m
  (median input))

(def average
  (->>
    input
    avg
    double
    Math/round))
; part1
(->>
  input
  (map #(Math/abs (- % m)))
  (reduce + 0)
  println)

; part2
(->>
  input
  (map #(reduce + 0 (range (inc (Math/abs (- % avg))))))
  (reduce + 0)
  println)
;  85015836 ; 462 -> 85016864

