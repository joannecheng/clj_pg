(ns aoc2022.day06
  (:require [clojure.java.io :as io]))

(defn marker?
  "Checks to see if the buffer is a start of packet marker"
  [buffer buf-size]
  (= (count (distinct buffer)) buf-size))

(defn update-buffer
  [buffer stream]
  (str (subs buffer 1) (first stream)))

(defn find-marker-pos [input buf-size]
  (loop [stream (subs input buf-size)
         buffer (subs input 0 buf-size)
         pos buf-size]
    (if (or (marker? buffer buf-size) (empty? stream))
      pos
      (recur (subs stream 1) (update-buffer buffer stream) (inc pos)))))

(find-marker-pos "bvwbjplbgvbhsrlpgdmjqwftvncz" 4) ;; 5
(find-marker-pos "nppdvjthqldpwncqszvftbrmjlhg" 4) ;; 6
(find-marker-pos "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4) ;; 10

(find-marker-pos (slurp (io/resource "aoc2022/day06.txt")) 4)

(find-marker-pos "bvwbjplbgvbhsrlpgdmjqwftvncz" 14) ;; 23
(find-marker-pos "nppdvjthqldpwncqszvftbrmjlhg" 14) ;; 23
(find-marker-pos "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14) ;; 29

(find-marker-pos (slurp (io/resource "aoc2022/day06.txt")) 14)
