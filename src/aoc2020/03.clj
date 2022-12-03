(ns aoc2020.03
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]))

(defn tree?
  [row pos]
  (= (nth (cycle row) pos) \#))

(defn trees-right-down1 [input right-n]
  (reduce
   (fn [coll row]
     (cond-> (update coll :x + right-n)
       (tree? row (:x coll)) (update :trees inc)))
   {:x 0 :trees 0}
   input))

(defn part2
  [input]
  (reduce
   (fn [coll {:keys [trees]}]
     (* coll trees))
   1
   [(trees-right-down1 input 1)
    (trees-right-down1 input 3)
    (trees-right-down1 input 5)
    (trees-right-down1 input 7)
    (trees-right-down1
     (->> (partition 2 2 input)
          (mapv first))
     1)]))

;; inputs
(def real-input
  (clojure.string/split
   (slurp
    (io/resource "aoc2020/day03.txt"))
   #"\n"))

(def sample-input-text
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def sample-input
  (clojure.string/split
   sample-input-text
   #"\n"))


;; part 1
(trees-right-down1 sample-input 3)
(trees-right-down1 real-input 1)

;; part 2
(part2 sample-input)
(part2 real-input)
