(ns advent.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :refer [is deftest testing]]
            [clojure.repl :refer :all]
            [instaparse.core :as insta]
            [instaparse.transform :refer [transform]]))

(defn load-sample []
  (->> "day04-sample.txt"
       io/resource
       slurp
       str/split-lines))

(defn load-data []
  (->> "day04-input.txt"
       io/resource
       slurp
       str/split-lines))

(def parser (insta/parser
             "CARD = <'Card'> <' '+> CARDNUMBER <':'> <' '+> WINNINGNUMBERS <' |'> <' '+>  YOURNUMBERS
              CARDNUMBER = #'[0-9]+'
              WINNINGNUMBERS = WINNINGNUMBER ( <' '+> WINNINGNUMBER )*
              WINNINGNUMBER = #'[0-9]+'
              YOURNUMBERS = YOURNUMBER ( <' '+> YOURNUMBER )*
              YOURNUMBER = #'[0-9]+'"))

(defn parse-input-line [line]
  (transform
   {:CARD vector
    :YOURNUMBER parse-long
    :YOURNUMBERS (comp set vector)
    :WINNINGNUMBER parse-long
    :WINNINGNUMBERS (comp set vector)
    :CARDNUMBER parse-long}
   (parser line)))

(defn correct-numbers [card]
  (apply set/intersection (rest card)))

(defn score [card]
  (->> (correct-numbers card)
       count
       dec
       (Math/pow 2)
       int))

(comment
  (tap> (parse-input-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))
  (tap> (-> (parse-input-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53") score))


  (tap> (reduce + (map score (map parse-input-line (load-sample))))) ;; 13
  (tap> (reduce + (map score (map parse-input-line (load-data))))) ;; redacted

;; End part one.
  )

(def sample-data (map parse-input-line (load-sample)))
(def data (map parse-input-line (load-data)))

(defn extra-cards [card]
  (->> (correct-numbers card)
       count))

