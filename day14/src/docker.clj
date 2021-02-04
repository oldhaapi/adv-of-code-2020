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

(defn mask-val-p1
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

(defn store-p1
  "[mem[1122] 4234] 111010XX11110X001110010XXXX10X110010"
  [[locstr sval] mask]
  (let [loc (Integer/parseInt (second (st/split locstr #"[\[\]]")))
        val (Integer/parseInt sval)
        mval (mask-val-p1 mask val)]
    [loc mval]))

(defn compute-p1
  [tape]
  (loop [mem {} mask 0 t tape]
    (if (empty? t)
        mem
        (let [inst (first t)
              ]
          (cond
            (= "mask" (first inst)) (recur mem (second inst) (rest t))
            :else
            (let [[l v] (store-p1 inst mask)]
              (recur (assoc mem l v) mask (rest t))))
          ))))

(defn solve
  [tape]
  (let [mem (compute-p1 tape)]
    (reduce + (vals mem)))
  )

(defn pow-b
  "Return fn taking b to the n power"
  [b]
  (fn [n] (reduce * (take n (repeat b)))))

(defn store
  [[locstr sval] mask]
  (let [loc (Integer/parseInt (second (st/split locstr #"[\[\]]")))
        val (Integer/parseInt sval)
        pow-2-fn (pow-b 2)
        xoffsets (filterv #(= \X (get mask (- 35 %))) (-> mask count range))
        bits (range (pow-2-fn (count xoffsets)))
                                        ; locz will end up with all
                                        ; '1' affected bits set to 1,
                                        ; and all X floating bits set
                                        ; to 0 (as (first bits) is always 0
        locz (reduce (fn [l b]
                       (let [mask-b (get mask (- 35 b))]
                         (cond
                           (= \1 mask-b) (bit-set l b)
                           (= \0 mask-b) l
                           (= \X mask-b) (bit-clear l b)))
                       ) loc (-> mask count range))
        ]
    (mapv (fn [nbin]
            (let [loc (reduce (fn [l i]
                                (let [b (bit-test nbin i)]
                                  (if b
                                    (bit-set l (get xoffsets i))
                                    l))) locz (range (count xoffsets)))]
              [loc val])
            ) bits)))

(defn compute
  [tape]
  (loop [mem {} mask 0 t tape]
    (if (empty? t)
      mem
      (let [inst (first t)]
        (cond
          (= "mask" (first inst)) (recur mem (second inst) (rest t))
          :else
          (let [kvs (store inst mask)
                ]
            (recur (apply assoc mem (flatten kvs)) mask (rest t))))))))

(defn solvep2
  [tape]
  (reduce + (vals (compute tape))))

(defn -main
  [& opts]
  (let [source (if (nil? opts) "puzz.txt" (first opts))
        tape (load-puzz source)
        ]
    (println "Part 1:" (solve tape))
    (println "Part 2 (looking for 4160009892257):" (solvep2 tape))))
