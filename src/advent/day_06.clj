(ns advent.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :refer [is deftest testing]]
            [clojure.repl :refer :all]
            [instaparse.core :as insta]
            [instaparse.transform :refer [transform]]))

;; I'm not going to write a parser for that small an input.  It's trivial in both cases.
(def sample [{:time 7 :distance 9}
             {:time 15 :distance 40}
             {:time 30 :distance 200}])

(def sample-2 {:time      71530
               :distance  940200})

(def data [{:time 44 :distance 283}
           {:time 70 :distance 1134}
           {:time 70 :distance 1134}
           {:time 80 :distance 1491}])

(def data-2 {:time 44707080
             :distance 283113411341491})
(def acceleration 1)

(defn distance-traveled [hold-time max-time]
  (let [speed (* hold-time acceleration)
        move-time (- max-time hold-time)]
    (* speed move-time)))

(defn winning-ms [{:keys [time distance]}]
  ;; ignore never start
  (let [hold-times (range 1 time)]
    (->> hold-times
         (filter #(> (distance-traveled % time) distance)))))

(defn winning-count
  "using reduce instead of count to avoid realizing the whole sequence when we only care about the number of items."
  [{:keys [time distance]}]
  ;; ignore never start
  (let [hold-times (range 1 time)]
    (->> hold-times
         (filter #(> (distance-traveled % time) distance))
         (reduce (fn [x _] (inc x)) 0))))

(defn score [data]
  (->>  data
        (map winning-ms)
        (map count)
        (reduce *)))

(deftest sample-test
  (testing "sample"
    (is (= (range 2 6) (winning-ms (first sample))))
    (is (= (range 4 12) (winning-ms (second sample))))
    (is (= (range 11 20) (winning-ms (nth sample 2)))))

  (testing "score"
    (is (= 288 (score sample)))))

(comment
  ;; part one
  (tap> (score data))

  ;; part two
  (tap> (time (winning-count sample-2)))
  (tap> (time (winning-count data-2))) ;; "Elapsed time: 1063.562625 msecs"
  ;; end comment
  )

(deftest part-two
  (is (= 71503 (winning-count sample-2))))
