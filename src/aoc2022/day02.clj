(ns aoc2022.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]))

(def sample-input
  "A Y
B X
C Z")

(def input (slurp (io/resource "aoc2022/day02.txt")))

;; A X Rock
;; B Y Paper
;; C Z Scissors

;; X means you need to lose
;; Y means you need to end round in a draw
;; Z means you need to win

(def match-points
  {["A" "X"] 3
   ["B" "Y"] 3
   ["C" "Z"] 3
   ["A" "Y"] 6
   ["A" "Z"] 0
   ["B" "X"] 0
   ["B" "Z"] 6
   ["C" "X"] 6
   ["C" "Y"] 0 })

(def lose-combos
  {"A" "Z"
   "B" "X"
   "C" "Y"})

(def draw-combos
  {"A" "X"
   "B" "Y"
   "C" "Z"})

(def win-combos
  {"A" "Y"
   "B" "Z"
   "C" "X"})

(defn shape-points [shape]
  (cond (= shape "X") 1
        (= shape "Y") 2
        :else 3))

(defn points-for-turn [shapes]
  (+ (shape-points (second shapes))
     (match-points shapes)))

(defn points-for-turn2
  [[opponent-shape output-shape]]
  (cond
    (= output-shape "X") (shape-points (get lose-combos opponent-shape))
    (= output-shape "Y") (+ (shape-points (get draw-combos opponent-shape))
                            (match-points [opponent-shape (get draw-combos opponent-shape)]))

    :else (+ (shape-points (get win-combos opponent-shape))
             (match-points [opponent-shape (get win-combos opponent-shape)]))))


(reduce + 0
        (map points-for-turn
             (->> (cstr/split input #"\n")
                  (map #(cstr/split % #" ")))))

(reduce + 0
        (map points-for-turn2
             (->> (cstr/split input #"\n")
                  (map #(cstr/split % #" ")))))

