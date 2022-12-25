(ns aoc2022.day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn char->int
  [c]
  (- (int c) 48))

(defn visible?
  [cur-tree tree-str]
  (every?
   true?
   (map
    #(> cur-tree (char->int %))
    tree-str)))

(defn scenic-count
  [cur-tree tree-str]
  (loop [tree-str tree-str
         c 0]
    (cond
      (empty? tree-str) c

      (> cur-tree (char->int (first tree-str)))
      (recur (rest tree-str) (inc c))

      :else (inc c))))

(defn bottom-to-edge
  [input x y return-fn]
  (let [cur-tree (-> input
                     (get-in [y x])
                     char->int)
        trees-to-edge (map #(get % x) (subvec input (inc y)))]
    (return-fn cur-tree trees-to-edge)))

(defn top-to-edge
  [input x y return-fn]
  (let [cur-tree (-> input
                     (get-in [y x])
                     char->int)
        trees-to-edge (map #(get % x) (subvec input 0 y))]
    (return-fn cur-tree (reverse trees-to-edge))))

(defn left-to-edge
  [input x y return-fn]
  (let [cur-tree (-> input
                     (get-in [y x])
                     char->int)
        trees-to-edge (-> input
                          (get y)
                          (subs 0 x))]
    (return-fn cur-tree (reverse trees-to-edge))))

(defn right-to-edge
  [input x y return-fn]
  (let [cur-tree (-> input
                     (get-in [y x])
                     char->int)
        trees-to-edge (-> input
                          (get y)
                          (subs (inc x)))]
    (return-fn cur-tree trees-to-edge)))

(defn part1
  [input]
  (let [result (for [x (range 0 (count (first input)))
                     y (range 0 (count input))]
                 (or
                  (right-to-edge input x y visible?)
                  (left-to-edge input x y visible?)
                  (bottom-to-edge input x y visible?)
                  (top-to-edge input x y visible?)))]
    (count (filter true? result))))

(defn part2
  [input]
  (let [result (for [x (range 0 (count (first input)))
                     y (range 0 (count input))]
                 (*
                  (right-to-edge input x y scenic-count)
                  (left-to-edge input x y scenic-count)
                  (bottom-to-edge input x y scenic-count)
                  (top-to-edge input x y scenic-count)))]
    (apply max result)))

(def sample-input
  "30373
25512
65332
33549
35390")

(def prepared-input
  (->> sample-input
       str/split-lines))

(def real-input
  (str/split-lines (slurp (io/resource "aoc2022/day08.txt"))))

(part1 prepared-input)
(part1 real-input)

(part2 prepared-input)
(part2 real-input)