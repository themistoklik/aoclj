(require ['clojure.string :as 'str])

(defn upper?
  [s]
  (every? #(Character/isUpperCase %) s))

(defn lower?
  [s]
  (every? #(Character/isLowerCase %) s))

(defn fmap
  "idiom to map to all vals of a map"
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/split-lines
    (map #(str/split % #"-"))
    (#(concat % (mapv reverse %)))
    (group-by #(first %))
    (fmap #(map second %))))

;part 1
(defn dfs
  [current path graph paths]
  (if (or (nil? current) (= "end" current))
    (conj paths path)
    (let [valid-neighs (filter #(or (upper? %) (not (contains? (set path) %))) (graph current))]
      (mapcat #(dfs % (concat path [%]) graph paths) valid-neighs))))

(println (count (dfs "start" ["start"] input [])))

;part 2

(defn never-seen-in-path?
  [path val]
  (not (contains? (set path) val)))

(defn dfs2
  [current path graph paths]
  (if (or (nil? current) (= "end" current))
    (conj paths path)
    (let [smalls (filter #(lower? %) (rest path))
          no-small-has-2-visits (every? true? (map #(> 2 %) (vals (frequencies smalls))))
          valid-neighs (filter #(or (upper? %) (never-seen-in-path? path %) (and (not= "start" %) no-small-has-2-visits)) (graph current))]
      (mapcat #(dfs2 % (concat path [%]) graph paths) valid-neighs))))

(println  (count (dfs2 "start" ["start"] input [] )))