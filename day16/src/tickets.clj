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
  "Return the invalid fields that are not a member of any rule ranges.
  "
  [all-ranges field]
  (empty? (filter (fn
                    [[mn mx]]
                    (and (>= field mn)
                         (<= field mx))) all-ranges)))


(defn solve
  "Find every invalid field in every nearby ticket and sum them."
  [tape]
  (let [all-ranges (apply concat (vals (:rules tape)))]
    (reduce + (filter (partial invalid? all-ranges) (apply concat (:nearby tape)))))
  )

(defn valid?
  [all-ranges ticket]
  (empty? (filter (partial invalid? all-ranges) ticket)))

(defn score
  "Return the field matching the slice"
  [rules tslice]
  (let [ifields (set (keys rules))
        mk1 (reduce clojure.set/intersection
                 (map #(reduce (fn [m k]
                                 (if (valid? (get rules k) [%])
                                   (conj m k)
                                   m)) #{} (keys rules)) tslice))
        ]
    mk1))

(defn field-pos
  "Create a map of fields to their positions in tickets by
  scoring each position in a ticket"
  [rules tickets]
  (reduce (fn [acc p]
            (let [slice (mapv #(nth % p) tickets)
                  mk (score rules slice)
                  confirmed (= 1 (count (seq mk)))
                  fd (filter #(not (contains? acc %)) (seq mk))
                  _ (if (> (count fd) 1)
                      (do (print "At pos" p",fd > 1:" )
                          (pp/pprint mk)
                          (println)))
                  ]
              (if confirmed
                (do (assoc acc (first mk) [p confirmed])
                    (println "Confirmed " (first mk) "at pos" p))
                (reduce #(apply assoc %1 %2
                                ;; If the key does not exist in acc or is not confirmed
                                ;; store the new value.
                                ) acc (for [f (seq mk)
                                        :let [[k v] [f [p confirmed]]
                                              [ap ac] (get acc f [nil false])]
                                        :when (not ac)]
                                    [k v]
                                    )))
              )) {} (range (count (first tickets)))))

(defn solvep2
  "P2 Part 1: Eliminate all invalid nearby tickets.
  P2 Part 2: Work out the order of the rules by position
  P2 Part 3: Multiple the six Departure fields in your ticket"
  [tape]
  (let [all-ranges (apply concat (vals (:rules tape)))
        valid-tickets (filter (partial valid? all-ranges) (:nearby tape))
        _ (println (count valid-tickets) "valid tickets")
        ordered-rules (field-pos (:rules tape) valid-tickets)
        _ (pp/pprint ordered-rules)
        dfields (filter #(re-find #"^depart" %) (keys ordered-rules))
        _ (pp/pprint dfields)
        _ (println (count dfields) "dfields")
        ]
    (reduce * (map (fn [fld]
                     (let [p (get ordered-rules fld)]
                       (nth (:your tape) p))) dfields))))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzz.txt" (first opts))
        tape (load-puzz source)
        rowcnt (count tape)
        _ (println "Counted " rowcnt "rows")
        ; _ (pp/pprint tape)
        ]
    (println "Part 1:" (solve tape))
    (println "Part 2:" (solvep2 tape))))
