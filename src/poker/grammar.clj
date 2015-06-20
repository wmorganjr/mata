(ns poker.grammar
  (:require [instaparse.core :as insta]
            [clojure.string :as string]))

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
           CARD = RANDOM | CARD_LIST | PAREN_CARD
           PAREN_CARD = '(' CARD ')'
           CARD_LIST = RANK | ANY_SUIT | RANK_PLUS | SUITED_RANK | RANK','CARD_LIST | RANK_PLUS','CARD_LIST | SUITED_RANK','CARD_LIST
           SUITED_RANK = RANK SUIT
           RANK_PLUS = RANK'+'
           RANK = %s
           ANY_SUIT = '*' SUIT
           SUIT = %s
           RANDOM = '*'"

          ranks-grammar
          suits-grammar))

(defn normalize-case
  [s]
  (-> s
      (string/replace #"[akqjt]" string/upper-case)
      (string/replace #"[SDHC]" string/lower-case)))

(def parser
  (comp (insta/parser grammar)
        normalize-case))
