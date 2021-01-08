(ns pdp8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-puzz
  "Tape data looks like the following:
  nop +0
  acc +1
  jmp +4
  acc +3
  jmp -3
  acc -99
  acc +1
  jmp -4
  acc +6

  Return vector of '(token integer) pairs
  "
  [source]
  (with-open [rdr (io/reader source)]
    (let [lines (line-seq rdr)
          _ (println "read" (count lines) "lines")]
      (mapv (fn [i]
             (let [[t nstr] (str/split i #" ")
                   n (Integer/parseInt nstr)]
               (cons t [n]))) lines))))
(def ip (atom 0))
(def acc (atom 0))
(def inst-set (atom #{}))

(defn mod_accum [n]
  (do
    (swap! acc + n)
    (swap! ip inc)
    @acc))
(defn mod_ip [n] (reset! ip (+ @ip n)))
(defn nop [n] (swap! ip inc))
(def imapf {"acc" mod_accum
            "jmp" mod_ip
            "nop" nop})

(defn do-op
  "Adjust instruction pointer (IP) and ACCumulator (acc)
  using input instructions/arg pairs.

  - acc increases or decreases a single global value called the accumulator by the value given in the argument. For example, acc +7 would increase the accumulator by 7. The accumulator starts at 0. After an acc instruction, the instruction immediately below it is executed next.inc
  - jmp jumps to a new instruction relative to itself. The next instruction to execute is found using the argument as an offset from the jmp instruction; for example, jmp +2 would skip the next instruction, jmp +1 would continue to the instruction immediately below it, and jmp -20 would cause the instruction 20 lines above to be executed next.
  - nop stands for No OPeration - it does nothing. The instruction immediately below it is executed next.

  "
  [instr]
  (let [[op x] instr
        _ (println "IP:" @ip op x)
        f (get imapf op)]
    (f x)))

(defn getnext [tape]
  (if (get @inst-set @ip false)
    (println "Acc is" @acc "on second execution of ip" @ip))
  (swap! inst-set conj @ip)
  (nth tape @ip))

(defn donext [tape]
  (println @ip)
  (do-op (getnext tape)))

(defn run
  [& opts]
  (let [source (if (nil? opts) "puzzleinput.txt" (first opts))
        tape (load-puzz source)
        _ (println "Counted " (count tape) "instructions")]
    (reset! ip 0)
    (reset! acc 0)
    (reset! inst-set #{})
    (loop []
      (if (not (contains? @inst-set @ip))
        (do
          (donext tape)
          (recur))
        (println "ACC is" @acc)))
    (println "Final accum:" @acc)))
