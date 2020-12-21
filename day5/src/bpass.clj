(ns bpass
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-puzz
  "Puzzle data:
  FBBFBBBRLR
  BFFBBFFLLL
  BFFBBBBRRR
  "
  [source]

  (with-open [r (io/reader source)]
    (let [lines (line-seq r)
          _ (println "Read" (count lines))]
      lines)))

(defn binpart
  "Given the inputs [upper lower] and 'F' and 'B' or 'L' and 'R',
  do the binary partitioning to converge on equal upper and lower bounds.
  F means use lower half, B means upper half.
  To be used in a reduction."
  [[l u] fb]
  (let [use-FL (or (= \F fb) (= \L fb))
        delta (quot (- u l) 2)
        mid (if use-FL
              (+ l delta)
              (- u delta))
        new-bnds (if use-FL
                   [l mid]
                   [mid u])
        ; _ (println fb mid new-bnds)
        ]
    new-bnds))

(defn find-seat-row
  "With first 7 chars, find row between 0 and 127"
  [bpass]
  (let [[l u] (reduce binpart [0 127] (take 7 bpass))]
    (if (not (= l u))
      (print "Hey! funny bpass:" bpass))
    l))

(defn find-seat-col
  "With last 3 chars, find seat column between 0 7"
  [bpass]
  (let [[l u] (reduce binpart [0 7] (nthrest bpass 7))]
    (if (not (= l u))
      (print "Hey! funny bpass:" bpass))
    l))

(defn seat-id
  "Every seat also has a unique seat ID: multiply the row by 8,
  then add the column. In this example, the seat has ID 44 * 8 + 5 = 357.
  "
  [bpass]
  (let [row (find-seat-row bpass)
        col (find-seat-col bpass)]
    (+ col (* row 8))))

(defn -main

  [& opts]
  (let [infile (if (nil? opts) "puzzleinput.txt" (first opts))
        puzdata (load-puzz infile)
        seat-ids (map seat-id puzdata)
        max-seat-id (apply max seat-ids)
        missing-id (apply max (filter #(not (contains? (set seat-ids) %)) (range max-seat-id)))]
    (println "Maximum seat ID is" max-seat-id, "missing SID:" missing-id)
    ))
