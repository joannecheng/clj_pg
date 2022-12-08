(ns aoc2022.day05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-command
  "returns num-crates, start, end"
  [row]
  (let [[_ num-crates start end] (re-find
                                  #"move (\d+) from (\d+) to (\d+)"
                                  row)]
    (mapv read-string [num-crates start end])))

(defn parse-row
  [row]
  (let [regex2 #"\[(\w)\]"]
    (loop [row row
           letters []]
      (let [crate (subs row 0 3)
            letter (last (re-find regex2 crate))]
        (if (= (count row) 3)
          (conj letters letter)
          (recur
           (subs row 4)
           (conj letters letter)))))))

(defn update-crates2
  [num-crates start end crates]
  (let [to-move (->> (get crates start)
                     (take num-crates))]
    (-> crates
        (update start #(drop num-crates %))
        (update end #(concat to-move %)))))

(defn update-crates
  [num-crates start end crates]
  (let [to-move (->> (get crates start)
                     (take num-crates)
                     reverse)]
    (-> crates
        (update start #(drop num-crates %))
        (update end #(concat to-move %)))))

(defn parse-procedure
  [rows crates update-crates-fn]
  (reduce
   (fn [coll row]
     (let [[num-crates start end] (parse-command row)]
       (update-crates-fn num-crates (dec start) (dec end) coll)))
   crates
   rows))

(defn divide-input
  [lines crate-linum procedure-linum update-crates-fn]
  (let [crates (->> (map parse-row (subvec lines 0 crate-linum))
                    (apply map list) ;; transpose the matrix
                    (mapv (comp #(remove nil? %)))) ;; removes nils
        procedure (subvec lines procedure-linum)]
    (parse-procedure procedure crates update-crates-fn)))

(defn find-answer
  [crates]
  (apply str (map first crates)))

(def real-input (slurp (io/resource "aoc2022/day05.txt")))

(def sample-input
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

;; Running
(-> (str/split-lines sample-input)
    vec
    (divide-input 3 5 update-crates)
    find-answer)

(-> (str/split-lines real-input)
    vec
    (divide-input 9 10 update-crates2)
    find-answer)