(ns aoc2022.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as w]))

(def sample-input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def cd-regex #"\$\scd\s(.+)")

(defn change-directory
  [fs commands]
  (reduce
   #(let [[_ dir] (re-matches cd-regex %2)]
      (if (= dir "..")
        (update %1 :curr pop)
        (update %1 :curr conj dir)))
   fs
   commands))

(defn command->dir-name
  [command]
  (last (re-matches #"dir (.+)" command)))

(defn command->size
  [filename-and-size]
  (read-string (first (str/split filename-and-size #"\n"))))

(defn update-parent-sizes
  "updates the 'parent' dirs with the new file size
   fs: the entire file system tree"
  [fs]
  (let [curr (:curr fs) ;; the current directory and its parents
        size (get-in fs (conj curr :size))]
    (loop [fs fs
           dirs (pop curr)
           last-size size]
      (if (seq dirs)
        (recur
         (update-in fs (conj dirs :size) + last-size)
         (pop dirs)
         size)
        fs))))

(get-in {:a 1} [:a :b])

(defn list-files
  [fs commands]
  (->>
   (rest commands)
   (reduce
    (fn [coll command]
      (let [dir-name (command->dir-name command)
            curr (:curr coll)]
        (if (and (some? dir-name) (nil? (get-in coll (conj curr dir-name))))
          (assoc-in coll (conj curr dir-name) {:size 0})
          (update-in coll (conj curr :size) + (command->size command)))))
    fs)
   update-parent-sizes))

(defn operate
  "Looks at first command of a command group"
  [fs command-group]
  (let [[_ dir] (re-matches cd-regex (first command-group))]
    (cond
      (some? dir) (change-directory fs command-group)
      :else (list-files fs command-group))))

(defn find-answer
  [fs]
  (let [sums (atom 0)]
    (w/postwalk (fn [x]
                  (when (and (coll? x)
                             (= (first x) :size))
                    (when (<= (second x) 100000)
                      (prn x)
                      (swap! sums #(+ % (second x)))))
                  x)
                fs)
    @sums))

(defn total
  [fs]
  (get-in fs ["/" :size]))

(defn find-answer-2
  [fs]
  (let [max-size 40000000
        total-size (total fs)
        dirs-to-delete (atom [])]
    (w/postwalk (fn [x]
                  (when (and (coll? x)
                             (= (first x) :size)) ;; is a colll
                    (when (<= (- total-size (second x)) max-size)
                      (swap! dirs-to-delete #(conj % (second x)))))
                  x)
                fs)

    @dirs-to-delete))

;;;; implementation

(def init-fs
  {"/" {:size 0}
   :curr []})

(->> sample-input
     str/split-lines
     (partition-by #(re-matches cd-regex %))
     (reduce operate init-fs)
     find-answer-2)

(def real-input (slurp (io/resource "aoc2022/day07.txt")))

(def real-fs
  (->> real-input
       str/split-lines
       (partition-by #(re-matches cd-regex %))
       (reduce operate init-fs)))

(->> real-fs
     find-answer-2
     (apply min))

;; 15835880 is too high
