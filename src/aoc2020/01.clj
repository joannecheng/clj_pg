(ns aoc2020.01
  (:require [clojure.java.io :as io]))

; https://adventofcode.com/2020/day/1/input

(def sample
  [1721
   979
   366
   299
   675
   1456])

(defn sets
  [data]
  (let [combinations (for [x data
                           y data]
                       (when (not= x y)
                         #{x y}))]
    (set (remove nil? combinations))))

(defn sets3
  [data]
  (let [combinations (for [x data
                           y data
                           z data]
                       (when (and (not= x y) (not= x z) (not= y z))
                         #{x y z}))]
    (set (remove nil? combinations))))

(defn find2020-sum
  [dataset]
  (-> (filter #(= (reduce + 0 %) 2020) dataset)
      first))

(defn product
  [selected-set]
  (reduce * 1 selected-set))

(defn find-product [list]
  (-> list
      sets
      find2020-sum
      product))

(defn find-product3 [list]
  (-> list
      sets3
      find2020-sum
      product))

(defn str-list->int-list
  [list]
  (mapv read-string list))

(def test-input (-> (slurp (io/resource "aoc2020/input01.txt"))
                    (clojure.string/split #"\n")
                    str-list->int-list))

(-> test-input
    sets3)

(find-product sample)
(find-product (-> (slurp (io/resource "aoc2020/input01.txt"))
                  (clojure.string/split #"\n")
                  str-list->int-list))

(find-product3 sample)
(find-product3 (-> (slurp (io/resource "aoc2020/input01.txt"))
                   (clojure.string/split #"\n")
                   str-list->int-list))
