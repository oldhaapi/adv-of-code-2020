(ns jolts
  (:require [clojure.java.io :as io]
            ))

(defn load-puzz
  "Input data is a list of integers
    "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)]
      (vec (sort (mapv #(Integer/parseInt %) lines))))))

(defn find-seps
  "Count instances of separation by n"
  [tape n]
  (let [first-diff (first tape)
        num (count
             (filterv #(= % n)
                      (map
                       (fn [idx]
                         (let [c (nth tape idx)
                               d (nth tape (inc idx) (+ c 3))]
                           (- d c))) (range (count tape)))))]
    (if (= first-diff n)
      (inc num)
      num)))



(defn s2
  "arr = [int(line.rstrip()) for line in        open('input.txt', 'r').readlines()]
  arr.sort()
  arr.append(arr[-1]+3)

  memo = {0: 1}
  for r in arr:
  memo[r] = memo.get(r-3,0)
          + memo.get(r-2,0)
          + memo.get(r-1,0)
  print(memo[arr[-1]])

  15790581481472
  "
  [tape]
  (let [last-ad (first (take-last 1 tape))
        jolts (conj (vec tape) (+ 3 last-ad))
        memo (loop [m {0 1} j (first jolts) jrest (rest jolts)]
               (let [mtmp (+ (get m (- j 3) 0)
                             (get m (- j 2) 0)
                             (get m (- j 1) 0))

                     ]
                 (if (empty? jrest)
                   m
                   (recur (assoc m j mtmp) (first jrest) (rest jrest)))))]
    ; (println memo)
    (get memo last-ad)))

(defn solve
  "arr = [int(line.rstrip()) for line in        open('input.txt', 'r').readlines()]
  arr.sort()
  arr.append(arr[-1]+3)

  memo = {0: 1}
  for r in arr:
  memo[r] = memo.get(r-3,0)
          + memo.get(r-2,0)
          + memo.get(r-1,0)
  print(memo[arr[-1]])

  15790581481472
  "
  [tape]
  (let [last-ad (first (take-last 1 tape))
        jolts (conj (vec tape) (+ 3 last-ad))
        memo (reduce (fn [m j]
                       (assoc m j (+ (get m (- j 3) 0)
                                     (get m (- j 2) 0)
                                     (get m (- j 1) 0))))
                     {0 1} jolts)]
    (get memo last-ad)))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        tape (load-puzz source)
        intcnt (count tape)
        _ (println "Counted " intcnt "integers")
        ]
    (println "Part 1:" (* (find-seps tape 1) (find-seps tape 3)))
    (println "Part 2:" (solve tape))))
