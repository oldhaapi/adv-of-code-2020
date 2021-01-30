(ns bus
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [flatland.ordered.map :as flatmap]
            [clojure.edn :as edn]
            ))

(defn load-puzz
  "1013728
  23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,733,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,449,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37
  "
  [source withx]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)
          earliest-ts (Integer/parseInt (first lines))
          busids (if withx (second lines) (mapv #(Integer/parseInt %) (filter #(not= "x" %) (st/split (second lines) #","))))]
      [earliest-ts busids])))

(defn get-buses
  "Return a map of busids and their offset to the first bus"
  [puzz]
  (let [ba (st/split puzz #",")]
    (reduce (fn [m i]
              (let [v (get ba i)
                    nm (if (= "x" v)
                         m
                         (assoc m (Integer/parseInt v) i))]
                nm)) (flatmap/ordered-map) (range (count ba)))))

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
  "Find first ts where all the buses depart at their subsequent offsets
  from the first bus -- line up the planets"
  [busmap]
  (let [_ (println busmap)
        lbus (bigint (last (keys busmap)))
        fbus (first (keys busmap))
        loffset (last (vals busmap))
        start-t (* fbus (quot 100000000000000N fbus))
        _ (println "Longest bus is" lbus "longest offset is" loffset "starting at t=" start-t)
        ]
    (first
     (filter
      (fn [t]
        (let [t-lbus (+ t (get busmap lbus))
              t-marker (mod t 100000000000000N)]
          (if (zero? t-marker)
            (println "t="t-marker))
          (if (zero? (mod t-lbus lbus))
            (every? zero? (map #(mod (+ t (get busmap %)) %) (keys busmap)))
            false)
          ))
      (iterate (partial + fbus) start-t)))))

(defn solve3
  "A proper part-2 solution from zengxinhui
  on the Reddit pages"
  []
  (let [gcd (fn [a b] (if (= 0 b) a (recur b (mod a b))))
        lcm (fn [a b] (/ (* a b) (gcd a b)))
        f (fn [n [_ s]]
            (if s (let [x (edn/read-string s)] [x (mod (- (* x (quot n x)) n) x)])))
        [timestamp & buses1] (->> (slurp "puzzleinput.txt") (re-seq #"\d+") (map edn/read-string))
        buses2 (->> (slurp "puzzleinput.txt")
                    (re-seq #"(\d+)|x")
                    rest
                    (keep-indexed f))]
    [(->> (map #(vector (- % (mod timestamp %)) %) buses1) sort first (reduce *))
     ((reduce (fn [[factor1 t] [factor2 remainder2]]
                (loop [t t]
                  (if (= (mod t factor2) remainder2)
                    [(lcm factor1 factor2) t]
                    (recur (+ t factor1))))) buses2) 1)]))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        [ts busids] (load-puzz source false)
        _ (println "Timestamp is" ts "busids:" busids)
        busmap (get-buses (second (load-puzz source true)))
        ]
    (println "Part 1:" (solve ts busids))
    (println "Part 2 (from reddit):" (solve3))
    (println "Part 2:" (solvep2 busmap))))
