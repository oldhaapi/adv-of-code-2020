(ns fix-exp
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (io/reader source)]
      (edn/read (java.io.PushbackReader. r)))
    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))

(defn mkpair
  "Loop to find pair that sum to goal and return them. Return empty list
  if collection is drained"
  [s goal]
  (loop [f (first s) rst (rest s)]
    (let [g (first (filter #(= goal (+ f %)) rst))]
      (if (empty? rst)
        rst
        (if (nil? g)
          (recur (first rst) (rest rst))
          [f g])))))

(defn mktriple
  [s goal]
  (loop [f (first s) rst (rest s)]
    (let [subgoal (- 2020 f)
          pair (mkpair rst subgoal)]
      (if (empty? rst)
        rst
        (if (empty? pair)
          (recur (first rst) (rest rst))
          (cons f pair))))))

(defn expenserpt
  "From items (integers) find the pair that sum to 2020 and return their product"
  [items]
  (reduce * (mktriple items 2020)))

(defn run [& opts]
  ;; Find the three numbers that sum to 2020 and then multiply them.
  (let [infile (if (nil? opts) "puzzleinput1.edn" (first opts))
        puzdata (load-edn infile)]
    (println (expenserpt puzdata)))
  )
