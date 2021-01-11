(ns xmas
  (:require [clojure.java.io :as io]
            ))

(defn load-puzz
  "Input data is 1000 big integers
    "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)]
      (mapv #(bigint %) lines))))

(def sz-preamble 25)

(defn make-sums
  "Make set of all the sums of pairs of ints in the preamble-sized
  subset of tape prior to idx"
  [tape idx]
  (let [start (- idx sz-preamble)
        ints (take sz-preamble (nthrest tape start))]
    (set (flatten (map (fn [i]
                         (let [x (nth ints i)]
                           (map #(+ x %) (nthrest ints i))))
                       (range sz-preamble))))))

(defn solve
  "Look for the number that is NOT in the set of sums of the previous preamble"
  [tape]
  (loop [idx sz-preamble]
    (let [n (nth tape idx)
          s (make-sums tape idx)]
      (if (contains? s n)
        (recur (inc idx))
        (do
          (println "Found solution:" n "at idx" idx)
          (println "Set:\n"s)
          n)))))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        tape (load-puzz source)
        intcnt (count tape)
        _ (println "Counted " intcnt "integers")
        ]
    (solve tape)))
