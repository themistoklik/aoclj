
(require ['clojure.string :as 'str]
         ['clojure.set :as 'set])
(use 'clojure.data)

(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/split-lines
    (mapv #(str/split % #"\|"))))

(defn in-uniques
  [n]
  (contains? #{2 4 3 7} n))

; does one completely envelop the other? order matters in the segment screen
(defn is-inside?
  [one other]
  (nil? (second (diff (set one) (set other)))))

(defn not-inside?
  [one other]
  (not (is-inside? one other)))

(def count-to-seg
  {2 1 3 7 4 4 7 8})

(defn get-0-6-9
  [group seven four]
  (let [nine (first (filter #(is-inside? % four) group))
        six (first (filter #(not-inside? % seven) group))
        zero (first (disj group nine six))]
    (zipmap (map set [zero six nine]) [0 6 9]))
  )

(defn get-2-3-5
  [group seven six]
  (let [five (first (filter #(is-inside? six %) group))
        three (first (filter #(is-inside? % seven) group))
        two (first (disj group three five))
        ]
    (zipmap (map set [two three five]) [2 3 5])))

(defn parse-pt2
  [ins outs]
  (let [cnts (group-by count ins)
        one (first (cnts 2))
        four (first (cnts 4))
        seven (first (cnts 3))
        eight (first (cnts 7))
        zero-six-nine (get-0-6-9 (set (cnts 6)) seven four)
        six ((set/map-invert zero-six-nine) 6)
        two-three-five (get-2-3-5 (set (cnts 5)) seven six)
        encodeds (merge (zipmap (map set [one four seven eight]) [1 4 7 8]) zero-six-nine two-three-five)] ; get 3 different
    (map #(encodeds %) (map set outs))
    )
  )

; part 1
(->>
  input
  (map (comp (partial str/triml) second))
  (map #(str/split % #" "))
  (map #(map count %))
  (map #(filter in-uniques %))
  (reduce concat)
  count
  println)


;part 2
(->>
  input
  (map #(vector (str/split (str/trimr (first %)) #" ") (str/split (str/triml (last %)) #" ")))
  (map #(parse-pt2 (first %) (second %)))
  (map (comp #(Integer/parseInt %) str/join))
  (reduce + 0)
  println
  )
