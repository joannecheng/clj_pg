(ns aoc2022.day09
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn input->steps
  "takes sample input string
   and returns list of directions and steps
   ex: 'R 4' -> ['R' 4]"
  [input]
  (->> input
       str/split-lines
       (map #(let [[dir num] (str/split % #" ")]
               [dir (read-string num)]))))

(defn touching?
  [[hx hy] [tx ty]]
  (and
   (and (<= tx (inc hx))
        (>= tx (dec hx)))
   (and (<= ty (inc hy))
        (>= ty (dec hy)))))

(defn move-head-1
  [head-pos dir]
  (cond
    (= dir "R") (update head-pos 0 inc)
    (= dir "L") (update head-pos 0 dec)
    (= dir "D") (update head-pos 1 inc)
    :else (update head-pos 1 dec)))

(defn move-diagonal-towards-head
  [new-head-pos tail-pos]
  (let [update-y-fn (fn [tail-pos]
                      (if (> (second new-head-pos) (second tail-pos))
                        ;; if the head y is larger than tail y, then increase y pos
                        (update tail-pos 1 inc)
                        (update tail-pos 1 dec)))
        update-x-fn (fn [tail-pos]
                      (if (> (first new-head-pos) (first tail-pos))
                        ;; if the head x is larger than tail x, then increase x pos
                        (update tail-pos 0 inc)
                        (update tail-pos 0 dec)))]
    
    (-> tail-pos
        update-y-fn
        update-x-fn)))

(defn move-lateral-towards-head
  [new-head-pos tail-pos]
  (if (= (first new-head-pos) (first tail-pos))
    (if (> (second new-head-pos) (second tail-pos))
      (update tail-pos 1 inc)
      (update tail-pos 1 dec))

    (if (> (first new-head-pos) (first tail-pos))
      (update tail-pos 0 inc)
      (update tail-pos 0 dec))))

(defn move-tail-1
  [new-head-pos tail-pos]
  (if (touching? new-head-pos tail-pos)
    tail-pos
    (cond
      ;; not diagonal, on same plane/same direction movement
      (or (= (second new-head-pos) (second tail-pos))
          (= (first new-head-pos) (first tail-pos)))
      (move-lateral-towards-head new-head-pos tail-pos)

      ;; diagonal movements
      :else (move-diagonal-towards-head new-head-pos tail-pos))))

(defn take-steps
  "Returns new head pos, new tail pos, and visited"
  ([head-pos tail-pos visited [dir steps]]
   (take-steps head-pos tail-pos visited [dir steps] true))

  ([head-pos tail-pos visited [dir steps] collect-visited?]
   (loop [head-pos head-pos
          tail-pos tail-pos
          visited visited ;; set
          steps-remaining steps]
     (if (= 0 steps-remaining)
       [head-pos tail-pos visited]
       (let [new-head-pos (move-head-1 head-pos dir)
             new-tail-pos (move-tail-1 new-head-pos tail-pos)]
         (recur new-head-pos
                new-tail-pos
                (cond-> visited
                  collect-visited? (conj new-tail-pos))
                (dec steps-remaining)))))))

(defn travel-bridge
  [steps]
  (loop
   [head-pos [0 0]
    tail-pos [0 0]
    visited #{}
    steps steps]
    (if (empty? steps)
      visited
      (let [[new-head new-tail new-visited] (take-steps head-pos tail-pos visited (first steps))]
        (recur new-head new-tail new-visited (rest steps))))))

(defn move-1-tail-nodes
  ;; moved: list of moved nodes (init: move head node)
  ;; unmoved: list of tail-nodes
  [moved unmoved dir]
  (let [last-moved (last moved)
        tail-node (first unmoved)]
    (cond
      (empty? unmoved) moved

      (touching? last-moved tail-node)
      (move-1-tail-nodes (conj moved tail-node) (rest unmoved) dir)

      :else
      (move-1-tail-nodes (conj moved
                               (move-tail-1
                                last-moved
                                tail-node))
                         (rest unmoved)
                         dir))))

(defn move-nodes
  [nodes-to-move [dir num]]
  (loop [counter num
         nodes nodes-to-move
         visited #{}]
    (if (= counter 0)
      [nodes visited]
      (let [moved-nodes (move-1-tail-nodes [(move-head-1 (first nodes) dir)]
                                           (rest nodes)
                                           dir)]
        (recur
         (dec counter)
         moved-nodes
         (conj visited (last moved-nodes)))))))

(defn travel-bridge-2
  [steps]
  (let [nodes [[0 0] [0 0] [0 0]
               [0 0] [0 0] [0 0]
               [0 0] [0 0] [0 0]
               [0 0]]]
    (reduce
     (fn [{:keys [new-nodes visited]} cur-step]
       (let [[moved-nodes visited-from-round] (move-nodes new-nodes cur-step)]
         {:new-nodes moved-nodes
          :visited (set/union visited visited-from-round)}))
     {:new-nodes nodes
      :visited #{(last nodes)}}
     steps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sample-input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def real-input
  (slurp (io/resource "aoc2022/day09.txt")))

(-> sample-input
    input->steps
    travel-bridge
    count)

(-> real-input
    input->steps
    travel-bridge
    count)

(def sample-input2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(-> sample-input2
    input->steps
    travel-bridge-2
    (get :visited)
    count)

(-> real-input
      input->steps
      travel-bridge-2
      (get :visited)
      count)
