(ns bags
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-puzz
  "Puzzle data is a set of rules of the form:
  light red bags contain 1 bright white bag, 2 muted yellow bags.
  dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  bright white bags contain 1 shiny gold bag.
  muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  faded blue bags contain no other bags.
  dotted black bags contain no other bags.

  Return map of bag rules
  "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)
          _ (println "read" (count lines) "lines")]
      (into (sorted-map) (map (fn [l]
                                (let [[nc1 nc2 _ _ & noderules] (str/split l #" ")
                                      nodecolor (str/join "-" [nc1 nc2])
                                      rulecnt (/ (count noderules) 3)
                                      rules (for [r (partition 4 noderules)
                                                  :let [[num c1 c2 _] r]]
                                              {(keyword (str/join "-" [c1 c2]))  (Integer/parseInt num)})]
                                  {(keyword nodecolor) (into {} rules)}))
                              lines)))))

(defn find-gold
  "Search for shiny-gold, return true if found"
  [pzd, k]
  (let [node (get pzd k)]
    (and (not (nil? node))
         (or (get node :shiny-gold)
             (some #(find-gold pzd %) (keys node))))))

(defn find-shiny-gold
  "find all paths to shiny-gold bags"
  [rules]
  (count (filter #(not (nil? %)) (map #(find-gold rules %) (keys rules))))
)


(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        rules-map (load-puzz source)
        _ (println "Counted " (count rules-map) "rules")]
    (println "Found " (find-shiny-gold rules-map) "ways to pack shiny-gold bags")))
