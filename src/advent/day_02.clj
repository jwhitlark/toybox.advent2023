(ns advent.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :refer [is deftest testing]]
            [clojure.repl :refer :all]
            [instaparse.core :as insta]
            [instaparse.transform :refer [transform]]))

(defn load-data []
  (-> "day02-input.txt"
      io/resource
      slurp
      str/split-lines))

(def max-cubes {:red 12 :green 13 :blue 14})

(def parser (insta/parser
             "GAME = <'Game '> GAMENUMBER <':'> <' '> HANDS
              HANDS = HAND ( <'; '> HAND )*
              HAND = CARD ( <', '> CARD )*
              GAMENUMBER = #'[0-9]+'
              CARD = CNT <' '> COLOR
              CNT = #'\\d+'
              COLOR = 'red' | 'green' | 'blue'
              "))

(defn parse-input-line [line]
  (transform
   {:GAMENUMBER parse-long
    :CNT parse-long
    :COLOR keyword
    :CARD (comp reverse vector)
    :HAND (comp (partial apply hash-map) flatten vector)
    :HANDS (comp set vector)
    :GAME hash-map}
   (parser line)))

;; Test data
(def x "Game 19: 6 red, 9 green; 1 blue, 4 red; 12 green; 5 green, 2 blue, 9 red")

(def sample-input
  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(comment
  ;; single line
  (tap> (parse-input-line x))

  ;; sample
  (tap> (map parse-input-line sample-input))

  ;; end comment
  )

;; compare each hand to the max-cubes
(defn possible-hand? [limit hand]
  (every? (fn [[color cnt]]
            (<= cnt (limit color)))
          hand))

(defn possible-game? [limit game]
  (every? (partial possible-hand? limit)
          (-> game val)))

(defn filter-possible-games [limit games]
  (into {} (filter #(possible-game? limit %) games)))

(defn part-one [limit games]
  (->> games
       (filter-possible-games limit)
       (keys)
       (reduce +)))

(comment
  (tap> [:possible-hand? (possible-hand? max-cubes {:red 6 :green 9})])

  (tap> (into {} (filter #(possible-game? max-cubes %) {1 #{{:red 6 :green 9}, {:blue 1, :green 2}}
                                                        2 #{{:red 6 :green 210}, {:blue 1, :green 2}}})))

  (tap> (part-one max-cubes (into {} (map parse-input-line sample-input))))
   ;; 8

  (tap> (part-one max-cubes (into {} (map parse-input-line (load-data)))))
   ;; <redacted>

  ;; end part one
  )

;; Should have done this in the first place.
(def inputs (into {} (map parse-input-line (load-data))))
(def sample-inputs (into {} (map parse-input-line sample-input)))

(defn merge-hands [game]
  (apply merge-with max (val game)))

(defn part-two [the-inputs]
  (reduce + (map #(->> % merge-hands (map val) (reduce *))
                 the-inputs)))

(comment ;; part two
  (tap> (part-two sample-inputs)) ;; => 2286

  (tap> (part-two inputs)) ;; => <redacted>

  ;; all in one go ;-P
  (tap> (reduce + (map #(->> %
                             val
                             (apply merge-with max)
                             (map val)
                             (reduce *))
                       (into {}
                             (map parse-input-line (load-data))))))

  ;; end comment
  )