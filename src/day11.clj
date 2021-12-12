(require ['clojure.string :as 'str]
         ['clojure.set :as 'set])

(defn mz [x] (* x 0))

(defn update-vals [m vals f]
  (reduce #(update % %2 f) m (filter not-empty vals)))

(defn chars-to-ints
  [cs]
  (map #(Character/digit % 10) cs))

(defn
  valid-coords
  [new-xy grid]
  (contains? grid new-xy))


(def input
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    str/split-lines
    (mapv #(apply list %))
    (mapv chars-to-ints)))

(defn neighbors
  ([yx g]
   (neighbors [[-1 0] [1 0] [0 -1] [0 1] [1 1] [1 -1] [-1 1] [-1 -1]]
              yx g))
  ([deltas yx g]
   (let [applied-deltas (map #(vec (map + yx %)) deltas)]
     (filter #(valid-coords % g) applied-deltas))))

(def all-coords
  (for [x (range (count input))
        y (range (count (first input)))]
    [x y]))

(def ins
  (zipmap all-coords (mapcat identity input)))

(defn inc-some-by-1
  [g coords]
  (update-vals g coords inc))


(defn inc-all-by-1
  [g]
  (inc-some-by-1 g all-coords))

(defn find-flashers
  [g]
  (filter #(< 9 (g %)) all-coords))

(defn reset-flashers
  [flasher-coords g]
  (update-vals g flasher-coords mz))

(defn inc-flasher-neighs
  [g to-flash af]
  (let [ns (filter #(not (or (contains? (set af) %) (contains? (set to-flash) %))) (mapcat #(neighbors % g) to-flash))]
    (inc-some-by-1 g ns)))

(defn apply-flash
  [to-flash g already-flashed]
  (->>
    g
    (#(reset-flashers to-flash %))
    (#(inc-flasher-neighs % to-flash already-flashed))
    ))

(defn flash
  [g]
  (loop [ng g
         already-flashed #{}]
    (let [to-flash (filter #(not (contains? (set already-flashed) %)) (find-flashers ng))]
      (if (empty? to-flash)
        ng
        (recur (apply-flash to-flash ng already-flashed) (concat already-flashed to-flash))))))

(defn step
  [g]
  (->> g
       inc-all-by-1
       flash))

; part1
(loop [times 0
       board ins
       flashes 0]
  (if (= times 100)
    (println flashes)
    (let [new-board (step board)
          zeroes ((frequencies (vals new-board)) 0 0)]
      (recur (inc times) new-board (+ zeroes flashes)))))

; part 2
(loop [times 0
       board ins]
  (if (every? true? (map #(= 0 %) (vals board)))
    (println times)
    (let [new-board (step board)]
      (recur (inc times) new-board))))