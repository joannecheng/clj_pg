(ns aoc2022.day04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn diff
  [elf]
  (- (second elf) (first elf)))

(defn range-contains?
  [larger-range smaller-range]
  (and (>= (first smaller-range) (first larger-range))
       (<= (second smaller-range) (second larger-range))))

(defn fully-contains?
  [[elf1 elf2]]
  (if (> (diff elf1) (diff elf2))
    (range-contains? elf1 elf2)
    (range-contains? elf2 elf1)))

(defn range-partial-contains?
  [larger-range smaller-range]
  (or
   (and
    (>= (first smaller-range) (first larger-range))
    (<= (first smaller-range) (second larger-range)))
   (and
    (>= (second smaller-range) (first larger-range))
    (<= (second smaller-range) (second larger-range)))))

(comment
  "6-6", "4-6")

(defn partial-contains?
  [[elf1 elf2]]
  (if (> (diff elf1) (diff elf2))
    (range-partial-contains? elf1 elf2)
    (range-partial-contains? elf2 elf1)))

(defn parse-line
  [line]
  (->> (str/split line #",")
       (map #(->> (str/split % #"-")
                  (map read-string)))))

(defn part1 [input]
  (->> (str/split-lines input)
       (map (comp fully-contains? parse-line))
       (filter true?)
       count))

(defn part2 [input]
  (->> (str/split-lines input)
       (map (comp partial-contains? parse-line))
       (filter true?)
       count))

(def sample-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(def real-input
  (slurp (io/resource "aoc2022/day04.txt")))

(part1 sample-input)
(part1 real-input)

(part2 sample-input)
(part2 real-input)
