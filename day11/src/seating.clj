(ns seating
  (:require [clojure.java.io :as io]
            [clojure.string :as st             ]
            ))

(defn load-puzz
  "Input row data:
  #.##.L#.##
  #L###LL.L#
  L.#.#..#..
  #L##.##.L#
  #.##.LL.LL
  #.###L#.##
  ..#.#.....
  #L######L#
  #.LL###L.L
  #.#L###.##
    "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)]
      (mapv #(st/split % #"") lines))))

(defn floor?
  "Return true if floor or out of range"
  [area [x y]]
  (= "." (get (get area x []) y ".")))

(defn seat-empty?
  "If x or y out of range, return floor (true)"
  [area [x y]]
  (not (= "#" (get (get area x []) y ".")))
  )

(defn occupied?
  "if x or y out of range return floor (false)"
  [area [x y]]
  (= "#" (get (get area x []) y "."))
  )


(def king-moves [[-1 -1] [-1 0] [-1 1]
                 [ 0 -1]        [ 0 1]
                 [1  -1] [ 1 0] [ 1 1]])

(defn adj-coords
  [[x y]]
  (mapv (fn [[u v]] [(+ x u) (+ y v)]) king-moves))

(defn look
  "Apply the rules to the seat, return L or #"
  [area x y]
  (let [adj (count (filter #(occupied? area %) (adj-coords [x y])))
        seat (get (get area x) y)]
    (if (floor? area [x y])
      seat
      (if (and (occupied? area [x y]) (>= adj 4))
        "L"
        (if (and (seat-empty? area [x y]) (= adj 0))
          "#"
          seat)))))

(defn seat-count
 [area]
 (count (for [x (range (count area))
              y (range (count (first area)))
              :when (occupied? area [x y])]
          [x y])))

(defn solve
  [area]
  (loop [a area scnt -1 iters 0]
    (let [newcnt (seat-count a)
          ;_ (clojure.pprint/pprint a)
          ]
      (if (= newcnt scnt)
        (do
          (println "iters:" iters)
          newcnt)
        (recur (mapv (fn [row]
                       (mapv #(look a row %) (range (count (first area)))))
                     (range (count area)))
               newcnt
               (inc iters))))))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        tape (load-puzz source)
        rowcnt (count tape)
        _ (println "Counted " rowcnt "rows")
        ]
    (println "Part 1:" (solve tape))
    (println "Part 2:" )))
