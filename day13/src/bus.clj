(ns bus
  (:require [clojure.java.io :as io]
            [clojure.string :as st             ]
            ))

(defn load-puzz
  "1013728
  23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,733,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,449,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37
  "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)
          earliest-ts (Integer/parseInt (first lines))
          busids (mapv #(Integer/parseInt %) (filter #(not= "x" %) (st/split (second lines) #",")))]
      [earliest-ts busids])))

(defn solve
  [ts busids]
  (let [blines (sort
                (mapv (fn [b]
                        (vector
                               (first
                                 (drop-while #(<= % ts)
                                             (iterate (partial + b) 0))) b)) busids))
        [ets busid] (first blines)]
    (println "Bus" busid "arriving at ts" ets)
    (* busid (- ets ts))))

(defn solvep2
  [ts busids]
  )

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        [ts busids] (load-puzz source)
        _ (println "Timestamp is" ts "busids:" busids)
        ]
    (println "Part 1:" (solve ts busids))
    (println "Part 2:" (solvep2 ts busids))))
