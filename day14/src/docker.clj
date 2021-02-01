(ns docker
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            ))

(defn load-puzz
  "mask = 110X1XX01011X100XX001X00100100X11X10
  mem[36932] = 186083
  mem[61779] = 1736
  mem[8438] = 233922
  mem[14437] = 52044
  mask = 111010XX11110X001110010XXXX10X110010
  "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)
          ]
      (println "Read" (count lines) "lines")
      (mapv #(st/split % #" = ") lines))))

(defn mask-val
  [mask val]
  ;(println mask val)
  (reduce (fn [nval b]
            (let [mask-bit (keyword (str (get mask (- 35 b))))
                  ;_ (print nval " ")
                  ;_ (print mask-bit)
                  opmap {:X (fn [] (if (bit-test val b)
                                      (bit-set nval b)
                                      (bit-clear nval b)))
                         :1 (fn [] (bit-set nval b))
                         :0 (fn [] (bit-clear nval b))}
                  op (get opmap mask-bit "bogus")
                  newval (op)
                  ]
              newval)) val (range (count mask))))

(defn store
  "[mem[1122] 4234] 111010XX11110X001110010XXXX10X110010"
  [[locstr sval] mask]
  (let [loc (Integer/parseInt (second (st/split locstr #"[\[\]]")))
        val (Integer/parseInt sval)
        mval (mask-val mask val)]
    [loc mval]))

(defn compute
  [tape]
  (loop [mem {} mask 0 t tape]
    (if (empty? t)
        mem
        (let [inst (first t)
              ]
          (cond
            (= "mask" (first inst)) (recur mem (second inst) (rest t))
            :else
            (let [[l v] (store inst mask)]
              (recur (assoc mem l v) mask (rest t))))
          ))))

(defn solve
  [tape]
  (let [mem (compute tape)]
    (reduce + (vals mem)))
  )

(defn solvep2
  [tape])

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzz.txt" (first opts))
        tape (load-puzz source)
        ]
    (println "Part 1:" (solve tape))
    (println "Part 2:" (solvep2 tape))))
