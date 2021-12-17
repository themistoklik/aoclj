(require ['clojure.string :as 'str])

(def inp
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    (re-seq #"target area: x=(\d+)..(\d+), y=(-\d+)..(-\d+)")
    ((fn [[[_ x-start x-end y-start y-end]]]
       {:x-start (read-string x-start) :x-end (read-string x-end) :y-start (read-string y-start) :y-end (read-string y-end)}))
    ))

;part1
(def deepest-y
  (min (inp :y-start) (inp :y-end)))

; Imagine standing in the middle of the box only shooting upwards
; you start at some y. the distance the ball travels is then [y,y-1,y-2,...,0],-1,-2,... all the way back to your y.
; the bracketed part gives you the highest point the ball is ever gonna reach and that's a known formula (triangle number or sth)
; now you just gotta figure out what's the best y. What's the lowest point of the bounding box you can hit like this without being out of it?
; It's the deepest y. If you went lower than that you could go at a higher y but you would be out of the box in the end.
(println (quot (* deepest-y (+ deepest-y 1)) 2))


;part 2 hello bruteforce my ooold frieeend, at least we know max y that's sth

(defn inside-box?
  [box x y]
  (let [{box-x-start :x-start box-x-end :x-end box-y-start :y-start box-y-end :y-end} box]
    (and (<= box-x-start x box-x-end) (<= box-y-start y box-y-end))))

(defn never-gonna-hit?
  "can't hit a box if you're past over it by x or past under it by y"
  [box x y]
  (let [{box-x-end :x-end box-y-start :y-start} box]
    (or (> x box-x-end) (< y box-y-start))))

(defn get-vx
  [vx]
  (cond
    (> vx 0) (dec vx)
    (< vx 0) (inc vx)
    :else 0))

(defn step
  [x y vx vy box]
  (cond
    (never-gonna-hit? box x y) 0
    :else (if (inside-box? box x y)
            1
            (step (+ vx x) (+ vy y) (get-vx vx) (dec vy) box))))

; lil bit hacky i play with the vy range until the sum doesn't change when i increase it
; OH WELL

(println
  (reduce + 0
          (map #(step 0 0 (first %) (second %) inp)
               (for [vx (range 1 (inc (inp :x-end)))
                     vy (range -300 300)]
                 [vx vy]))))