(ns game
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            ))

(defn load-puzz
  "
  0,13,1,16,6,17
  "
  []
  [0,13,1,16,6,17]
  ;[0 3 6]
  )

(defn solve
  "Starting with the last number in the vector:
  If first time the number is spoken, say 0,
  else say the how many turns ago when the number was spoken.

  Return lazy sequence of turns.
  "
  [tape maxturn]
  (let [tapelen (count tape)]
    (let [mem (reduce (fn [a i] (assoc a (get tape i) (inc i))) (array-map) (range tapelen))
          _ (println mem)
          spoke 0
          [_ rez] (reduce (fn [[mem spoken] i]
                            (let [turn i
                                  agediff (- turn (get mem spoken 0))
                                  _ (if (<= turn 15)
                                      (println "t:"turn spoken))]
                              (if (= turn maxturn)
                                [{} spoken]
                                (if (contains? mem spoken)
                                  [(assoc mem spoken turn) agediff]
                                  [(assoc mem spoken turn) 0])))
                            ) [mem spoke] (nthrest (take maxturn (iterate inc 1)) tapelen))]
      rez)))

(defn -main
  [& opts]
  (let [tape (load-puzz)
        ]
    (println "Part 1:" (solve tape 2020))
    (println "Part 2:" (solve tape 30000000))))
