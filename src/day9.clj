
(require ['clojure.string :as 'str]
         ['clojure.set :as 'set])

(defn chars-to-ints
  [cs]
  (map #(Character/digit % 10) cs))

(defn
  valid-coords
  [[newx newy] sizex sizey]
  (and (<= 0 newx (dec sizex)) (<= 0 newy (dec sizey)))
  )

(defn neighbors
  ([sizex sizey yx]
   (neighbors [[-1 0] [1 0] [0 -1] [0 1]]                   ; define neighbors to be 1 spot away, crosswise
              sizex
              sizey
              yx))
  ([deltas sizex sizey yx]
   (let [applied-deltas (map #(vec (map + yx %)) deltas)]
     (filter #(valid-coords % sizex sizey) applied-deltas))))



(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/split-lines
    (mapv #(apply list %))
    (mapv chars-to-ints)
    to-array-2d))

(defn check-smaller-than-neighs
  [val ns]
  (if (every? true? (mapv #(< val %) ns)) (inc val) 0))

(def size-y
  (count (first input)))

(def size-x
  (count input))


(let [smaller-than-neighs-risks (for [x (range size-x)
                                      y (range size-y)]
                                  (let [neighs (neighbors size-x size-y [x y])
                                        neigh-vals (map #(aget input (first %) (second %)) neighs)]
                                    (check-smaller-than-neighs (aget input x y) neigh-vals)
                                    ))]
  (println (reduce + 0 smaller-than-neighs-risks)))

(defn neighbors-no-9
  [node visiteds]
  (let [valid-neighbors (filter #(not (contains? visiteds %)) (neighbors size-x size-y node))]
    (filter #(not= 9 (aget input (first %) (second %))) valid-neighbors)))

(defn bfs-eager
  [visited [x y]]
  (loop [ret []
         queue (conj clojure.lang.PersistentQueue/EMPTY [x y])
         vs visited]
    (if (not= 0 (count queue))
      (let [node (peek queue)]
        (recur (conj ret node) (apply conj (pop queue) (vec (neighbors-no-9 node vs))) (conj vs node)))

      [ret vs])))



(def all-pos (conj [] (for [x (range size-x)
                            y (range size-y)]
                        [x y]
                        )))

(def part-2
  (loop
    [visited #{}
     ps (first all-pos)
     res []]
    (let [x (first (first ps))
          y (second (first ps))]
      (if (seq ps)
        (let [run (if (or (= 9 (aget input x y)) (contains? visited [x y])) [[] visited] (bfs-eager visited [x y]))]
          (recur (second run) (rest ps) (conj res (first run))))
        res))))

(->>
  part-2
  (filter not-empty)
  (map distinct)
  (map count)
  sort
  reverse
  (take 3)
  (reduce * 1)
  println)
