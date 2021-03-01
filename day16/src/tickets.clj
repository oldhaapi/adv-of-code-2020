(ns tickets
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            ))

(defn read-lines
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)
          [rules _ your _ nearby] (partition-by #(re-find #"^$" %) lines)]
      (println "Read" (count lines) "lines")
      {:rules rules
       :your (second your)
       :nearby (rest nearby)})))

(defn load-puzz
  "class: 1-3 or 5-7
  row: 6-11 or 33-44
  seat: 13-40 or 45-50

  your ticket:
  7,1,14

  nearby tickets:
  7,3,47
  40,4,50
  55,2,20
  38,6,12
 "
  [source]
  (let [sections (read-lines source)
        rules (let [krs (map #(str/split % #": ") (:rules sections))
                    rkeys (map first krs)
                    raw-ranges (map #(re-seq #"\d+" %) (map second krs))
                    ranges (map #(partition 2 %) (partition 4 (map #(Integer/parseInt %) (flatten raw-ranges))))
                    ]
                (zipmap rkeys ranges))
        your (mapv #(Integer/parseInt %) (re-seq #"\d+" (:your sections)))
        nearby (mapv (fn [ts]
                       (->> ts
                            (re-seq #"\d+")
                            (map #(Integer/parseInt %)))) (:nearby sections))
        ]
    {:rules rules
     :your your
     :nearby nearby
     }))

(defn invalid?
  "Return the invalid fields thar are not a member of any rule ranges.
  "
  [all-ranges field]
  (empty? (filter (fn
                    [[mn mx]]
                    (and (>= field mn)
                         (<= field mx))) all-ranges)))

(defn solve
  "Find every invalid field in every nearby ticket and sum them."
  [tape]
  (let [all-ranges (apply concat (map #(get (:rules tape) %) (keys (:rules tape))))]
    (reduce + (filter #(invalid? all-ranges %) (apply concat (:nearby tape)))))
  )

(defn solvep2
  [tape]
  )

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzz.txt" (first opts))
        tape (load-puzz source)
        rowcnt (count tape)
        _ (println "Counted " rowcnt "rows")
         _ (pp/pprint tape)
        ]
    (println "Part 1:" (solve tape))
    (println "Part 2:" (solvep2 tape))))
