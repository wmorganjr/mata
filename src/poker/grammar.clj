(ns poker.grammar
  (:require [instaparse.core :as insta]))

(def ranks '[A K Q J T 9 8 7 6 5 4 3 2])
(def suits '[s d h c])

(def cards
  (set (for [rank ranks
             suit suits]
         (str rank suit))))

(def ranks-grammar
  (apply str (interpose \| (map #(str \' % \') ranks))))

(def suits-grammar
  (apply str (interpose \| (map #(str \' % \') suits))))

(def grammar
  (format "HAND = CARD*
           CARD = SUITED_RANK | RANDOM | RANK_PLUS | RANK_LIST
           RANK_LIST = RANK | RANK','RANK_LIST
           SUITED_RANK = RANK SUIT
           RANK_PLUS = RANK'+'
           RANK = %s
           SUIT = %s
           RANDOM = '*'"

          ranks-grammar
          suits-grammar))

(def parser
  (insta/parser
    grammar))



