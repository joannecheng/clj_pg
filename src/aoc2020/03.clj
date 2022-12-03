(ns aoc2020.03)


(def sample-input
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

(defn trees-right3-down1 [input]
  (reduce
   (fn [coll row]
     (if (= (nth (cycle row) (:x coll)) \#)
       (-> coll
           (update :x + 3)
           (update :trees inc))
       (update coll :x + 3)))
   {:x 0 :trees 0}
   (clojure.string/split input #"\n")))

(trees-right3-down1 sample-input)