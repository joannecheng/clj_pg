(ns aoc2022.day10 
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; 20th, 60th, 100th, 140th, 180th, and 220

(defn add-operation
  "Takes an operation string (ex: 'addx -4') and the last machine state
   and returns the 2 cycles needed to perform operation
   ex: 
   ['addx -4' 1] 
   returns [1 -3]"
  [op curr-state]
  (let [op-num (-> (str/split op #" ")
                   last
                   read-string)]
    [curr-state (+ curr-state op-num)]))

(defn find-cycles
  [operations]
  (reduce
   (fn [reg-state op]
     (if (= op "noop")
       (conj reg-state (last reg-state))
       (apply conj reg-state (add-operation op (last reg-state)))))
   [1]
   operations))

(defn find-cycle-strength
  [cycle-vals cycle-num]
  (* cycle-num (get cycle-vals (dec cycle-num))))

(defn find-cycle-strengths
  [cycle-vals]
  (+ (find-cycle-strength cycle-vals 20)
     (find-cycle-strength cycle-vals 60)
     (find-cycle-strength cycle-vals 100)
     (find-cycle-strength cycle-vals 140)
     (find-cycle-strength cycle-vals 180)
     (find-cycle-strength cycle-vals 220)))

(defn sprite-overlap?
  [x cycle-num]
  (let [pos (mod (dec cycle-num) 40)]
    (and (<= x (inc pos))
         (>= x (dec pos)))))

(defn draw-pixel
  "X: current register value
   pos: position of pixel"
  [x cycle-num]
  (let [pixel (if (sprite-overlap? x cycle-num)
                "#"
                ".")]
    pixel))

(defn draw-pixels
  [operations]
  (loop
   [cycle 1
    crt-str ""
    x 1
    operations operations]
    (prn "operation" (first operations))
    (let [curr-op (first operations)
          next-x (if (= "noop" curr-op)
                   x
                   (last (add-operation curr-op x)))
          next-cycle (if (= "noop" curr-op)
                       (inc cycle)
                       (+ 2 cycle))
          curr-str (if (= "noop" curr-op)
                     (str crt-str (draw-pixel x cycle))
                     (str crt-str
                          (draw-pixel x cycle)
                          (draw-pixel x (inc cycle))))]

      (if (= 1 (count operations))
        curr-str
        (recur next-cycle
               curr-str
               next-x
               (rest operations))))))

(defn print-output [lines]
  (doseq [line lines]
    (println (apply str line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def sample-input
  "noop
addx 3
addx -5")

(def sample-input2
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(-> sample-input2
    str/split-lines
    find-cycles
    find-cycle-strengths)

(-> (io/resource "aoc2022/day10.txt")
    slurp
    str/split-lines
    find-cycles
    find-cycle-strengths)

(->> (io/resource "aoc2022/day10.txt")
     slurp
     str/split-lines
     draw-pixels
     (partition 40)
     print-output)

(->> sample-input2
     str/split-lines
     draw-pixels
     (partition 40))
 