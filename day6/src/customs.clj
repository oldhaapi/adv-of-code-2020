(ns customs
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-puzz
  "Puzzle data:
  clvxybjp
  kripatlzy

  yrn
  labirzd
  noypr

  groups are separated by blank lines

  Return: list of sequences of strings delimited by blank lines
  "
  [source]

  (with-open [r (io/reader source)]
    (let [lines (line-seq r)
          _ (println "Read" (count lines))]
      lines)))

(defn -main
  "For each group, count the number of questions to which anyone
  answered 'yes'. What is the sum of those counts?"
  [& opts]
  (let [infile (if (nil? opts) "puzzleinput.txt" (first opts))
        puzdata (load-puzz infile)]
    ))
