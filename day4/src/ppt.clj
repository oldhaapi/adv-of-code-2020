(ns ppt
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

(defn validate-pp
  "Validate passport data. 8 fields is valid, or 7 ifonly cid is
  missing."
  [dataf]
  (let [fldcnt (count (keys dataf))]
    (or (= 8 fldcnt)
        (and (= 7 fldcnt) (not (contains? dataf :cid))))))

(defn valid-yr?
  [fld lbnd ubnd]
  (and (= 4 (count fld))
       (try
         (let [yr (Integer/parseInt fld)]
           (and (<= yr ubnd)
                (>= yr lbnd)))
         (catch NumberFormatException e
           false))))

(defn valid-hgt?
  [pd]
  (let [num (Integer/parseInt (re-find #"\d+" pd))
        units (re-find #"[^\d]+" pd)]
    (if (= "cm" units)
      (and (>= num 150) (<= num 193))
      (and (>= num 59) (<= num 76)))))

(defn valid-hcl?
  [pd]
  (if (and (= 7 (count pd))
           (= "#" (str (first pd))))
    (try
      (and (Integer/parseInt (clojure.string/join (rest pd)) 16) true)
      (catch NumberFormatException e
        false))
    false))

(defn valid-ecl?
  [pd]
  (let [colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}]
    (contains? colors pd))
  )

(defn valid-pid?
  [pd]
  (and (= 9 (count pd))
       (try
         (and (Integer/parseInt pd) true)
         (catch NumberFormatException e
           false))))

(defn ppvalid?
  "
  You can continue to ignore the cid field, but each other field has strict rules about what values are valid for automatic validation:

  byr (Birth Year) - four digits; at least 1920 and at most 2002.
  iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  hgt (Height) - a number followed by either cm or in:
  If cm, the number must be at least 150 and at most 193.
  If in, the number must be at least 59 and at most 76.
  hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  pid (Passport ID) - a nine-digit number, including leading zeroes.
  cid (Country ID) - ignored, missing or not.
  "
  [ppdata]
  (and (valid-yr? (:byr ppdata) 1920 2002)
       (valid-yr? (:iyr ppdata) 2010 2020)
       (valid-yr? (:eyr ppdata) 2020 2030)
       (valid-hgt? (:hgt ppdata))
       (valid-hcl? (:hcl ppdata))
       (valid-ecl? (:ecl ppdata))
       (valid-pid? (:pid ppdata))))

(defn -main [& opts]
  ;; Find the three numbers that sum to 2020 and then multiply them.
  (let [infile (if (nil? opts) "puzzleinput.edn" (first opts))
        puzdata (load-edn infile)
        goodfields (filter validate-pp puzdata)
        fullvalids (filter ppvalid? goodfields)]
    (println (count goodfields) "good" (count fullvalids) "fully valid")))
