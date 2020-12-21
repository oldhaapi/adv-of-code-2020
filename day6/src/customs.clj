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

  Return: list of sequences of strings
  "
  [source]

  (with-open [r (io/reader source)]
    (let [lines (line-seq r)
          _ (println "Read" (count lines))]
      (loop [grps '() ls lines]
        (let [gs (take-while #(not (re-find #"^$" %)) ls)]
          (if (empty? gs)
            (reverse grps)
            (recur (cons gs grps) (nthrest ls (inc (count gs))))))))))

(defn -main
  "For each group, count the number of questions to which anyone
  answered 'yes'. What is the sum of those counts?"
  [& opts]
  (let [infile (if (nil? opts) "puzzleinput.txt" (first opts))
        puzdata (load-puzz infile)
        counts-yes (reduce + (map #(-> (str/join %) set vals count) puzdata))
        count-common-yes (reduce + (map #(-> (apply clojure.set/intersection (map set %)) count) puzdata))]
    (println "Count =" counts-yes)
    (println "Common count =" count-common-yes)))
