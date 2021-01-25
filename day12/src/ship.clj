(ns ship
  (:require [clojure.java.io :as io]
            [clojure.string :as st             ]
            ))

(defn load-puzz
  "Input row data:
  F10
  N3
  F7
  R90
  F11
  "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)]
      (mapv (fn [l]
              [(str (first l))
               (Integer/parseInt (second (st/split l #"[^\d+]")))]) lines))))

(defn bearing
  "Compute new bearing. East is bearing 0, 90 is N, etc. "
  [news rl d]
  (let [qtrs-l {0 "E"
                90 "N"
                180 "W"
                270 "S"
                360 "E"}
        qtrs {"E" 0
              "N" 90
              "W" 180
              "S" 270}
        b (get qtrs news)
        nb (if (= rl "L")
             (mod (+ b d) 360)
             (mod (+ 360 (- b d)) 360))]
    ; (println "b:" b "nb:" nb)
    (get qtrs-l nb)))

(defn docmd
  [[x y b] [c d]]
  (let [newpos (cond
                 (= c "N") [x (+ y d) b]
                 (= c "E") [(+ x d) y b]
                 (= c "S") [x (- y d) b]
                 (= c "W") [(- x d) y b]
                 (= c "R") [x y (bearing b c d)]
                 (= c "L") [x y (bearing b c d)]
                 (= c "F") (docmd [x y b] [b d]))
        ;_ (println "I:" [c d] [x y b]  "O:" newpos)
        ]
    newpos))

(defn solve
  [tape]
  (let [[x y b] (reduce docmd [0 0 "E"] tape)
        ax (if (< x 0) (- x) x)
        ay (if (< y 0) (- y) y)]
    (+ ax ay)))

(defn rotate-waypoint
  ""
  [x y c d]
  (if (= "R" c)
    (cond
      (= d 90) [y (- x)]
      (= d 180) [(- x) (- y)]
      (= d 270) [(- y) x]
      :else [x y])
    (cond
      (= d 90) [(- y) x] ; 10 4 -> -4 10
      (= d 180) [(- x) (- y)]
      (= d 270) [y (- x)]
      :else [x y])))

(defn dowaypoints
  [[x y wx wy] [c d]]
  (let [newpos (cond
                 (= c "N") [x y wx (+ wy d)]
                 (= c "E") [x y (+ wx d) wy]
                 (= c "S") [x y wx (- wy d)]
                 (= c "W") [x y (- wx d) wy]
                 (= c "R") [x y (rotate-waypoint wx wy c d)]
                 (= c "L") [x y (rotate-waypoint wx wy c d)]
                 (= c "F") [(+ x (* wx d)) (+ y (* wy d)) wx wy])]
    ; (println "I:" [c d] [x y wx wy] "O:" newpos)
    (flatten newpos)))

(defn solvep2
  [tape]
  (let [[x y wx wy] (reduce dowaypoints [0 0 10 1] tape)
        ax (if (< x 0) (- x) x)
        ay (if (< y 0) (- y) y)]
    (+ ax ay)))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        tape (load-puzz source)
        rowcnt (count tape)
        _ (println "Counted " rowcnt "rows")
        ]
    (println "Part 1:" (solve tape))
    (println "Part 2:" (solvep2 tape))))
