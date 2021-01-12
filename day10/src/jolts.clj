(ns jolts
  (:require [clojure.java.io :as io]
            ))

(defn load-puzz
  "Input data is a list of integers
    "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)]
      (sort (mapv #(Integer/parseInt %) lines)))))

(defn find-adapter
  "Find an lowest adapter within 3 jolts of j"
  [tape j]
  (first (filter #(contains? #{1 2 3} (- % j)) tape)))

(defn find-ns
  "Count instances of separation by n"
  [tape n]
  (let [first-diff (first tape)
        num (count
             (filterv #(= % n) (map
                                (fn [idx]
                                  (let [c (nth tape idx)
                                        d (nth tape (inc idx) (+ c 3))]
                                    (- d c))) (range (count tape)))))]
    (if (= first-diff n)
      (inc num)
      num)))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        tape (load-puzz source)
        intcnt (count tape)
        _ (println "Counted " intcnt "integers")
        ]
    (println "Part 1:" (* (find-ns tape 1) (find-ns tape 3)))
    (println "Part 2:" )))
