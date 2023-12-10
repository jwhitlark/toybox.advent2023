(ns advent.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :refer [is deftest testing]]
            [clojure.repl :refer :all]
            [instaparse.core :as insta]
            [instaparse.transform :refer [transform]]))

(def sample-1-raw
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def sample-2-raw
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(def main-data
  (->> "day08-input.txt"
       io/resource
       slurp))

(def parser
  (insta/parser
   "TOP = DIRS | PATH
DIRS = DIR*
DIR = L | R
L = 'L'
R = 'R'
PATH = LOC <' = ('> LOC <', '> LOC <')'>
LOC = LOCCHAR LOCCHAR LOCCHAR
LOCCHAR = #'[A-Z]'"))

(defn parse-input-line [line]
  (transform
   {:L (constantly 0)
    :R (constantly 1)
    :DIR identity
    :DIRS  vector
    :LOCCHAR str
    :LOC (comp keyword str)
    :PATH (fn [a b c] (hash-map a [b c]))
    :TOP identity}
   (parser line)))

(defn load  [data]
  (let [all (map parse-input-line (str/split-lines data))
        path (first all)
        loc (drop 2 all)]
    {:path path
     :loc (apply merge loc)}))

(defn step [loc curr-step next-step]
  (get-in loc [curr-step next-step]))

(defn find-path [{:keys [path loc]}]
  (reduce (fn [x y]
            (let [c (:step x)
                  nxt (step loc c y)]
              (if (= nxt :ZZZ)
                (reduced (inc (:steps x)))
                {:steps (inc (:steps x))
                 :step nxt})))
          {:steps 0 :step :AAA}
          (cycle path)))

(defn solve-part-1 []
  (find-path (load main-data)))

(comment
  (tap> (find-path (load sample-1-raw))) ;; 2
  (tap> (find-path (load sample-2-raw))) ;; 6
;; end comment
  )