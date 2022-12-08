(ns aoc2020.04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]))

(def rules
  {#{"byr" "iyr" "eyr" "hgt"
     "hcl" "ecl" "pid" "cid"} true
   #{"byr" "iyr" "eyr" "hgt"
     "hcl" "ecl" "pid"} true})

(defn passports
  [input]
  (clojure.string/split input #"\n\n"))

(defn passport-fields
  [passport]
  (->> (str/split passport #"\s")
       (map #(let [[_ code value] (re-find #"(\w{3}):(\S+)" %)]
               [code value]))))

(defn valid-passport?
  [parsed-passport]
  (get rules (set (map first parsed-passport)) false))

(s/def :passport/byr #(s/and (>= % 1920)
                             (<= % 2020)))
(s/def :passport/iyr #(s/and (>= % 2010)
                             (<= % 2020)))
(s/def :passport/eyr #(s/and (>= % 2020)
                             (<= % 2030)))
(s/def :passport/hgt #(s/and (>= % 2020)
                             (<= % 2030)))

(comment
  (subs "abc123in" (- (count "abc123in") 2))
  (re-matches #"(\d+)(in|cm)" "12341in"))

(s/def :passport/cid (s/nilable string?))

(s/def ::passport
  (s/keys
   :req-un [::byr]
   :opt-un [::cid]))

(s/valid? ::passport {:byr 2000})

;;;;;;;;;
(def sample-input
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

;; inputs
(def real-input
  (slurp
   (io/resource "aoc2020/day04.txt")))

(->> (passports sample-input)
     (map (comp valid-passport? passport-fields))
     (filter true?)
     (count))

(->> (passports real-input)
     (map (comp valid-passport? passport-fields))
     (filter true?)
     (count))

