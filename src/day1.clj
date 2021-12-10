(defn pair-bigger-than-last
  [input]
  (->>
    input
    (partition 2 1)
    (map #(> (second %) (first %)))
    (filter identity)
    count))


; part 1
(println (pair-bigger-than-last input))


;part 2
(->>
  input
  (partition 3 1)
  (map #(reduce + 0 %))
  pair-bigger-than-last
  println)

