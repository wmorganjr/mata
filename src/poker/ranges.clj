(ns poker.ranges
  (:require [clojure.math.combinatorics :as combo]))

(def ranks (map str '[A K Q J T 9 8 7 6 5 4 3 2]))
(def suits (map str '[s d h c]))

(def cards
  (set (for [rank ranks
             suit suits]
         (str rank suit))))

(defmulti cards-in-range first)

(defmethod cards-in-range :RANK
  [[_ rank]]
  (filter #(.startsWith % rank) cards))

(defmethod cards-in-range :SUIT
  [[_ rank]]
  (filter #(.endsWith % rank) cards))

(defmethod cards-in-range :ANY_SUIT
  [[_ _ suit]]
  (cards-in-range suit))

(defmethod cards-in-range :PAREN_CARD
  [[_ _ [_ card] _]]
  (cards-in-range card))

(defmethod cards-in-range :CARD_LIST
  [[_ card-first _ card-rest]]
  (concat (cards-in-range card-first)
          (and card-rest (cards-in-range card-rest))))

(defmethod cards-in-range :RANK_PLUS
  [[_ [_ rank]]]
  (let [i (.indexOf ranks rank)]
    (filter #(<= (.indexOf ranks (.substring % 0 1)) i) cards)))

(defmethod cards-in-range :SUITED_RANK
  [[_ [_ rank] [_ suit]]]
  [(str rank suit)])

(defmethod cards-in-range :RANDOM
  [_]
  cards)

(defn fill-placeholders
  [placeholders]
  (loop [[ph & phs] (sort-by :cardinality placeholders)
         deck (shuffle cards)
         result []
         cards #{}]
    (if ph
      (let [card (first (filter #(and (not (cards %))
                                      (get (:range ph) %))
                                deck))]
        (recur phs
               (shuffle deck)
               (conj result (assoc ph :card card))
               (conj cards card)))
      result)))

(defn spec->phs
  [completed-spec]
  (for [[k v] completed-spec
        card-spec (rest v)
        :let [cc (cards-in-range (second card-spec))]]
    {:type k
     :range (set cc)
     :cardinality (count cc)}))
