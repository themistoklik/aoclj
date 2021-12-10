(require ['clojure.string :as 'str])


(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    (#(str/split % #","))
    (mapv read-string)))

(defn fish
  [l]
  (if (= 0 l) \r (dec l)))

(defn spawn
  [fs]
  (vec (concat fs (repeat ((frequencies fs) \r 0) 8))))


; part1 ez
(loop
  [fs input
   days 0]
  (if (= 80 days)
    (println (count fs))
    (recur (replace {\r 6} (spawn (mapv fish fs))) (inc days))))

; part2 from reddit, not ashamed, learnt something
(->> input
     (reduce #(update %1 %2 inc) (vec (repeat 9 0)))        ;freq array OMG
     (iterate
       #(let [nf (conj (vec (rest %)) (first %))]           ;slide lefttt
          (update nf 6 (partial + (get nf 8)))              ;zero is at the end now
          )
       )
     (#(nth % 256))                                         ;updates the map 256 times
     (apply +)
     (println)
     )