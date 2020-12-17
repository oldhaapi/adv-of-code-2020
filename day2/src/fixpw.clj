(ns fixpw
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

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

(defn min-max
  [mms]
  (let [[mn mx] (str/split mms #"-")]
    [(Integer/parseInt mn) (Integer/parseInt mx)]))

(defn load-puzz
  "Puzzle data:
  11-13 h: hphhhhhhhfhshhhhh
  1-4 l: hbljb

  map into {:minl n, :maxl m, :l letter, :pw: str}
  "
  [source]

  (with-open [r (io/reader source)]
    (let [lines (map #(str/split % #" ") (line-seq r))
          _ (println "Read" (count lines))
          ]
      (map (fn [ln]
             (let [[f1 f2 f3] ln
                   [mn mx] (min-max f1)]
               {:minl mn :maxl mx
                :l f2
                :pw f3}))
           lines))))

(defn goodpw-p1?
  "Filter password that pass criteria. Each entry is a
  map described in load-puzz"
  [pwmap]
  (let [freqs (frequencies (:pw pwmap))
        ltrfreq (get freqs (get (:l pwmap) 0) 0)]
    (and (>= ltrfreq (:minl pwmap)) (<= ltrfreq (:maxl pwmap)))))

(defn goodpw?
  "Filter with part 2 rules:
  * the minl field is a 1-based index
  * the maxl fields is a 1-based index
  * the :l letter must exist in just one of the index positions"
  [pwmap]
  (let [idx1 (- (:minl pwmap) 1)
        idx2 (- (:maxl pwmap) 1)
        idx1t (= (nth (:pw pwmap) idx1) (first (:l pwmap)))
        idx2t (= (nth (:pw pwmap) idx2) (first (:l pwmap)))]
    (and (or idx1t idx2t) (not (and idx1t idx2t)))))

(defn goodpw-count
  [pd]
  (count (filter goodpw? pd))
  )

(defn run [& opts]
  ;; Find the count of valid passwords
  (let [infile (if (nil? opts) "puzzleinput.txt" (first opts))
        puzdata (load-puzz infile)]
    (println (goodpw-count puzdata) "valid passwords")))
