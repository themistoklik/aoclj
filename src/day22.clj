(require ['clojure.string :as 'str])


(defn rangify
  [n1 n2]
  (let [x1 (read-string n1)
        x2 (read-string n2)]
    (vector (min x1 x2) (inc (max x1 x2)))))

(def inp
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/split-lines
    (mapv #(re-seq #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" %))
    (map first)
    (map rest)
    (map (fn [[cmd x1 x2 y1 y2 z1 z2]] [cmd (rangify x1 x2) (rangify y1 y2) (rangify z1 z2)]))
    ))

(defn bounded-at-50?
  [c]
  (every? #(<= -51 % 51) (reduce concat c)))

(def inp1
  (->> inp
       (filter #(true? (bounded-at-50? (rest %))))
       ))

(println (count (reduce (fn [acc [cmd [x1 x2] [y1 y2] [z1 z2]]]
                   (let [cubes (for [x (range x1 x2)
                                     y (range y1 y2)
                                     z (range z1 z2)]
                                 [x y z])]
                     (condp = cmd
                       "on" (apply conj acc cubes)
                       "off" (apply disj acc cubes))))
                 #{}
                 inp)))