(require ['clojure.string :as 'str])

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))

(def hex-to-bin
  {
   \0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(def ops
  {
   0 #(reduce +' 0 %)
   ;0 "+"
   1 #(reduce *' 1 %)
   ;1 "*"
   2 #(apply min %)
   ;2 "min"
   3 #(apply max %)
   ;3 "max"
   5 #(if (> (first %) (second %)) 1 0)
   ;5 "gt"
   6 #(if (< (first %) (second %)) 1 0)
   ;6 "lt"
   7 #(if (= (first %) (second %)) 1 0)
   ;7 "="
   })

(defn resolve-num
  [s start end]
  (Long/parseLong (subs s start end) 2))


(def in
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/trim
    (apply vector)
    (mapcat #(hex-to-bin %))
    str/join))

(defn parse-lit
  [s vals versions version]
  (let [groups (->>
                 s
                 (partition 5)
                 (take-until #(= \0 (first %))))
        bits-parsed (* 5 (count groups))
        val (->>
              groups
              (mapcat #(drop 1 %))
              str/join
              (#(Long/parseLong % 2)))]
    [(subs s bits-parsed) (conj vals val) (conj versions version)])
  )

(defn parse
  [s vals versions]
  (let [version (resolve-num s 0 3)
        type (resolve-num s 3 6)
        rest-s (subs s 6)]
    (cond
      (every? #(= % \0) s) [s vals versions]
      ;(and (< (count s) 6) (every? #(= % \0) s)) [s vals versions]
      (= type 4) (parse-lit rest-s vals versions version)
      :else (let [len-id (resolve-num rest-s 0 1)]
              (condp = len-id
                0 (loop [[rs vs vss] [(subs rest-s 16) [] (conj versions version)]]
                    (if (every? #(= % \0) rs)
                      (do
                        [rs (concat [((ops type) vs)] vals) vss])
                        ;[rs (concat [vs] vals) vss])
                      (recur (parse rs vs vss))))
                1 (loop [n 0
                         [rs vs vss] [(subs rest-s 12) [] (conj versions version)]]
                    (if (= n (Long/parseLong (subs rest-s 1 12) 2))
                      (do
                        [rs (concat  [((ops type) vs)] vals) vss])
                        ;[rs (concat  [vs] vals) vss])
                      (recur (inc n) (parse rs vs vss)))))))))

;;part 1,2
(println (reduce + 0 (nth (parse in [] []) 2)))
(println (parse in [] []))
