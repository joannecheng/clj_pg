(ns aoc2022.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]
            [clojure.set :as s]))

(defn points-for-letter
  [letter]
  (if (<= (int letter) 90)
    (- (int letter) 38)
    (- (int letter) 96)))

(defn find-common-letter
  [[group1 group2]]
  (first (s/intersection
          (set group1)
          (set group2))))

(defn find-common-letter2
  [[group1 group2 group3]]
  (first (s/intersection
          (set group1)
          (set group2)
          (set group3))))

(defn split-groups
  [line]
  (split-at (/ (count line) 2) line))

(defn split-groups2
  [input]
  (partition 3 3 input))

(defn part1
  [input]
  (->> (cstr/split input #"\n")
       (map (comp points-for-letter find-common-letter split-groups))
       (reduce + 0)))

(defn part2
  [input]
  (->> (cstr/split input #"\n")
       split-groups2
       (map (comp points-for-letter find-common-letter2))
       (reduce + 0)))

(def sample-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def real-input (slurp (io/resource "aoc2022/day03.txt")))

(part1 sample-input)
(part1 real-input)

(part2 sample-input)
(part2 real-input)