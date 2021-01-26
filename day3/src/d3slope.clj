(ns d3slope
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-puzz
  "Puzzle data:
  ......#..##..#...#...#.###.....
  #..#............#..........#...
  ..........#....#..........#....
  ....#..#.#..........#..#.....#.

  Return sequence of lines
  "
  [source]

  (with-open [r (io/reader source)]
    (let [lines (line-seq r)
          _ (println "Read" (count lines))
          ]
      lines)))

(defn tree?
  "Is there a tree at this position?"
  [pos track]
  (let [c (count track)
        posx (mod pos c)]
    (= (nth track posx) \#))
  )

(defn find-trees
  "Find trees given a slope"
  [hill, [x y]]
  (loop [trees 0 tx x hrst (nthrest hill y)]
    (if (empty? hrst)
      trees
      (recur (if (tree? tx (first hrst))
               (+ 1 trees)
               trees)
             (+ x tx)
             (drop y hrst)))))

(defn find-all-trees
  "Right 1, down 1.
  Right 3, down 1. (This is the slope you already checked.)
  Right 5, down 1.
  Right 7, down 1.
  Right 1, down 2.
"
  [hill]
  (let [slopelist [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (reduce * (map #(find-trees hill %) slopelist))))

(defn -main [& opts]
  ;; Find the count of trees down the slopes
  (let [infile (if (nil? opts) "puzzleinput.txt" (first opts))
        puzdata (load-puzz infile)
        slope [3 1]]
    (println (find-trees puzdata slope) "trees")
    (println "Trees for all slopes:" (find-all-trees puzdata))))
