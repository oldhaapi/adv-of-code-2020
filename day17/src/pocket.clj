(ns pocket
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            ))

(defn load-puzz
  "
  Return an object that reflects a cube
  of maxz layers with z=0, x=0, y=0 being the upper left
  point of the following 2d array:
##..#.#.
#####.##
#######.
#..#..#.
#.#...##
..#....#
....#..#
..##.#..
"
  [source maxz]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)
          _ (println (first lines))
          ydim (count lines)
          xdim (count (first lines))
          blank-layer (to-array-2d (partition xdim (take (* xdim ydim) (repeat false))))
          given (to-array-2d (partition xdim (keep #(if (= \. %) false true) (apply concat lines))))]
      (println "read" ydim "lines")
      (into {} (keep #(if (zero? %) {% given} {% blank-layer}) (range (- maxz) (inc maxz)))))))

(defn solve
  [tape]
  )

(defn solvep2
  [tape]
  )

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzz.txt" (first opts))
        tape (load-puzz source)
        rowcnt (count tape)
        _ (println "Counted " rowcnt "rows")
        ; _ (pp/pprint tape)
        ]
    (println "Part 1:" (solve tape))
    (println "Part 2:" (solvep2 tape))))
