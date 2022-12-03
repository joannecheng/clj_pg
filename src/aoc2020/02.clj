(ns aoc2020.02
  (:require [clojure.java.io :as io]))

(defn valid-line?
  [{:keys [min-count max-count letter input]}]
  (let [frequency (or (get (frequencies input) letter)
                      -1)]
    (and (>= frequency  min-count)
         (<= frequency max-count))))

(defn valid-line2?
  [{:keys [min-count max-count letter input]}]
  (let [pos1? (= (get input (dec min-count)) letter)
        pos2? (= (get input (dec max-count)) letter)]
    (cond
      (and pos1? pos2?) false
      (and pos1? (not pos2?)) true
      (and pos2? (not pos1?)) true
      (not (and pos1? pos2?)) false)))

(get "abnkfk" 0)

(defn split-line [line]
  (let [[_ min-count max-count letter input]
        (re-matches #"(\d+)-(\d+)\s(\w)\:\s(.*)" line)]
    {:min-count (read-string min-count)
     :max-count (read-string max-count)
     :letter (.charAt letter 0)
     :input input}))

(def input
  (-> (slurp (io/resource "aoc2020/input02.txt"))
      (clojure.string/split #"\n")))

(->> (map (comp valid-line2? split-line) input)
     (filter true?)
     count)

(comment
  (-> (split-line "1-3 a: abcde")
      valid-line2?)

  (-> (split-line "1-3 b: cdefg")
      valid-line2?)

  (-> (split-line "2-9 c: ccccccccc")
      valid-line2?)

  (.charAt "a" 0)

  (get (frequencies "abcccc") \c))