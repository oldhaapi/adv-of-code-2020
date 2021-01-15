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

(defn find-adapters
  "Find adapters within 3 jolts of j. Adjust for the last adapter -> device
  Do not include members of ignored set."
  [tape j & ignore]
  (let [last-ad (first (take-last 1 tape))
        device (+ 3 last-ad)
        acceptable (apply (partial disj #{1 2 3}) (first ignore))]
    (if (= j last-ad)
      [device]
      (filterv #(contains? acceptable (- % j )) tape))))

(defn find-adapters-rev
  "Find adapters within 3 jolts of j, looking backwards. Adjust for the
  last adapter -> wall. Do not include members of ignored set."
  [tape j & ignore]
  (let [last-ad (first (take-last 1 tape))
        first-ad (first tape)
        device (+ 3 last-ad)
        acceptable (apply (partial disj #{-1 -2 -3}) (first ignore))]
    (if (= j first-ad)
      [0]
      (filterv #(contains? acceptable (- % j )) (reverse tape)))))

(defn s2
  "What is the number of variations in adapter choices to reach
  from the wall (0) to the device (last adapter + 3 jolts)?"
  [tape]
  (let [goal (+ 3 (first (take-last 1 tape)))]
    (reduce + (count (find-adapters tape 0)) (map #(count (find-adapters tape %)) tape))))

(defn s3
  "
  from collections import OrderedDict as odict, Counter

  jolts = sorted(int(x) for x in open().read().strip().split()
jolts = [0] + jolts + [jolts[-1] + 3]

# Part 1.
counter = Counter(jolts[i+1] - jolt for i, jolt in enumerate(jolts[:-1]))
print counter[1] * counter[3]

# Part 2.
dag = odict([(x, {y for y in range(x+1, x+4) if y in jolts}) for x in jolts])

def dfc(D, v, M={}):

    if v in M:
        return M[v]
    elif D[v]:
        M[v] = sum(dfc(D, x, M) for x in D[v])
        return M[v]
    else:
        return 1

print dfc(dag, 0)  "
  [tape]

  (defn dfc
    [D v & M]
    (if (get M v)
      (get M v)
      (if (get D v)
        (reduce + (map #(dfc D % M) (get D v)))
        1)))

  (let [last-ad (first (take-last 1 tape))
        jolts (conj (vec (cons 0 tape)) (+ 3 last-ad))
        dag-proto (take (count tape)
                        (map (fn [x]
                               [x (set (find-adapters jolts x))]) jolts))
        dag (into (array-map) dag-proto)
        dfc-memo (memoize dfc)]
    (dfc-memo dag 0)
    )
  )

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

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        tape (load-puzz source)
        intcnt (count tape)
        _ (println "Counted " intcnt "integers")
        ]
    (println "Part 1:" (* (find-seps tape 1) (find-seps tape 3)))
    (println "Part 2:" (solve tape))))
