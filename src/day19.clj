(:require
  ['clojure.string :as 'str]
  ['clojure.set :as 'set])

(defn read-s
  [l]
  (map #(read-string (str "[" % "]")) l))

(defn sub-lil-from-big
  [a b]
  (let [lil (min (Math/abs a) (Math/abs b))
        big (max (Math/abs a) (Math/abs b))]
    (- big lil)))

(def inp
  (->>
    "/Users/themis/IdeaProjects/aoclj/src/src/input.txt"
    slurp
    (#(str/split % #"---.*---"))
    (map str/trim)
    (map #(str/split % #"\n"))
    ;(mapv (map #(read-string (str "[" % "]"))))
    rest
    (mapv read-s)
    ))

(defn get-xs
  [scanner]
  (first (apply map list scanner)))

(defn xs
  [scanners]
  (->>
    scanners
    (map get-xs)))


;; i ended up doing it in python after massive hints, did not like it at all!
