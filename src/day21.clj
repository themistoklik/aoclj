(require ['clojure.string :as 'str])

(def p1-start 1)
(def p2-start 2)

(def det-die (cycle (range 1 101)))

(defn resolve-new-pos
  [roll curr-pos]
  (nth (cycle (range 1 11)) (+ curr-pos (dec roll))))

(defn roll
  [die pos nrolls]
  (let [r (reduce + 0 (take 3 die))
        new-pos (resolve-new-pos r pos)]
    {:nrolls (+ nrolls 3) :die (drop 3 die) :new-pos new-pos :score new-pos}))

(defn play
  [p1 p2 s1 s2 d end-score]
  (loop [p1-pos p1
         p2-pos p2
         p1-score s1
         p2-score s2
         nrolls 0
         die d]
    (if (or (<= end-score p1-score) (<= end-score p2-score))
      (cond
        (< p1-score p2-score) (* nrolls p1-score)
        :else (* (- nrolls 3) (- p2-score p2-pos))
        )
      (let [p1-play (roll die p1-pos nrolls)
            p2-play (roll (p1-play :die) p2-pos (p1-play :nrolls))]
        (recur (p1-play :new-pos) (p2-play :new-pos) (+ p1-score (p1-play :score)) (+ p2-score (p2-play :score)) (p2-play :nrolls) (p2-play :die))))
    ))

;part1
(println (play p1-start p2-start 0 0 det-die 1000))

;part2 i pythoned again
;(loop [states {}
;       p1 p1-start
;       p2 p2-start
;       s1 0
;       s2 0
;       state [[p1 s1] [p2 s2]]]
;  (if (not (nil? (states state)))
;    (merge-with + states (states state))
;    (for [x (range 1 4)
;          y (range 1 4)]
;      (recur ()))
;    ))
;def play(p1_state, p2_state,curr_playing, states={}):
;
;if (p1_state, p2_state, curr_playing) in states:
;return states[(p1_state, p2_state,curr_playing)]
;
;
;dice_rolls = [sum(p) for p in product([1, 2, 3], repeat=3)]
;
;won = [0,0]
;for roll in dice_rolls:
;p1,p2 = p1_state
;s1,s2 = p2_state
;positions = [p1, p2]
;scores = [s1, s2]
;
;positions[curr_playing] = (positions[curr_playing] + roll - 1) % 10 + 1
;scores[curr_playing] += positions[curr_playing]
;
;if scores[curr_playing] >= 21:
;won[curr_playing] += 1
;else:
;curr_playing = 1 if curr_playing == 0 else 0
;wins = play_driac(
;                  [positions[0], scores[0]], [positions[1], scores[1]], curr_playing)
;
;won = [won[0] + wins[0], won[1]+wins[1]]
;
;states[(p1_state, p2_state,curr_playing)] = won
;return won
;
;print(max(play(1, 2)))