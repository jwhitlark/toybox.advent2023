(ns advent.day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :refer [is deftest testing]]
            [clojure.repl :refer :all]))


(defn load-data []
  (-> "day01-input.txt"
      io/resource
      slurp
      str/split-lines))

(def digits (set (map str (range 0 10))))

(defn find-calibration-parts
  "Find, join, and parse the first and last digit of a string."
  [s]
  (->> s
       (map str)
       (filter digits)
       ((juxt first last))
       str/join
       parse-long))

(comment  ;; This is Part One
  ;; look at the first five lines
  (take 5 (load-data))

  ;; test it out
  (find-calibration-parts "five6dlh33x1")

  ;; check a single one
  (-> (load-data)
      first
      find-calibration-parts)

  ;; check all of them (solution to Part One)
  (->> (load-data)
       (map find-calibration-parts)
       (reduce +))

  ;; end Part One
  )

(def written-numbers
  ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(def all-numbers
  (concat written-numbers (map str (range 1 10))))

(def digit-replacements
  (zipmap written-numbers (map str (range 1 10))))

(defn index-number
  "Find the first index of each written number in a string.
   "
  [s]
  ;; rather than write out the functions, we can generate them
  ;; so we don't have to write out the same thing nine times
  ;; this is the first one written by hand if you want to see what it looks like
  ;; (fn [s] [(str/index-of s "one") "one"])
  (let [index-fns (map #(fn [s] [(str/index-of s %) %])
                       all-numbers)]
    (->> s
         ((apply juxt index-fns))
         (remove #(nil? (first %))))))

(defn last-index-number
  "Find the last index of each written number in a string.
   "
  [s]
  (let [index-fns (map #(fn [s] [(str/last-index-of s %) %])
                       all-numbers)]
    (->> s
         ((apply juxt index-fns))
         (remove #(nil? (first %))))))

(defn lowest-index
  "Find the lowest index of a written number in a string.
   "
  [s]
  (->> s
       index-number
       (sort-by first)
       first))

(defn highest-index
  "Find the highest index of a written number in a string.
   "
  [s]
  (->> s
       last-index-number
       (sort-by first)
       last))

(defn find-calibration-parts-2
  [s]
  ((juxt lowest-index highest-index) s))

(defn calc-value [x]
  (if (> (count x) 1)
    (digit-replacements x)
    x))

(defn part-two-calibration [s]
  (->> s
       find-calibration-parts-2
       (map #(calc-value (second %)))
       (apply str)
       parse-long))

(comment
  (index-number "one1two2three3")

  (lowest-index "one1two2three3")

  (last-index-number "one1two2three3")

  (highest-index "one1two2three3")

  (find-calibration-parts-2 "one1two2three3")

  (calc-value "1")
  (calc-value "one")

  (part-two-calibration "one1two2three3")
    ;; end comment
  )


(comment ;; this is Part Two

  ;; Sample of part two
  (->> ["two1nine"
        "eightwothree"
        "abcone2threexyz"
        "xtwone3four"
        "4nineeightseven2"
        "zoneight234"
        "7pqrstsixteen"]
       (map part-two-calibration)
       (reduce +))

  ;; Solution to Part Two
  (->> (load-data)
       (map part-two-calibration)
       (reduce +))

  ;; end Part Two
  )