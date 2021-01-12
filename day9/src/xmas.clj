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

(defn solve-weaknum
  "Look for the number that is NOT in the set of sums of the previous preamble"
  [tape]
  (nth tape
       (first
        (filter
         #(not (contains? (make-sums tape %) (nth tape %)))
         (range sz-preamble (count tape))))))

(defn solve
  "Given the weak number, find the set of contiguous numbers that sum to
  it, and return the sum of the lowest + highest of those"
  [tape w]
  (first
   (filter #(not (zero? %))
           (map #(loop [sub 2]
                   (let [e (sort (take sub (nthrest tape %)))
                         sum (reduce + e)]
                     (if (= sum w)
                       (do
                         (println "Found" (count e) "elements at index" %)
                         (+ (first e) (first (take-last 1 e))))
                       (if (> sum w)
                         0
                         (recur (inc sub)))))                                           ) (range sz-preamble (count tape))))))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        tape (load-puzz source)
        intcnt (count tape)
        _ (println "Counted " intcnt "integers")
        weaknum (sw2 tape)]
    (println "Part 1:" weaknum)
    (println "Part 2:" (solve tape weaknum))))
