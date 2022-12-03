(ns aoc2022.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]))

(def sample-input
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn input->groups [input]
  (->> (cstr/split input #"\n")
       (partition-by #(= % ""))
       (remove #(= % '("")))
       (map #(map read-string %))))

(defn part1 [input]
  (->> (input->groups input)
       (map #(reduce + 0 %))
       (apply max)))

(defn part2
  "Find total of top 3"
  [input]
  (->> (input->groups input)
       (map #(reduce + 0 %))
       sort
       reverse
       (take 3)
       (reduce + 0)))

(def real-input
  (slurp (io/resource "aoc2022/day01.txt")))

(part1 sample-input)
(part1 real-input)

(part2 sample-input)
(part2 real-input)
;; 45000 
