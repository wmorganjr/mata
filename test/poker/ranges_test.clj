(ns poker.ranges-test
  (:require [midje.sweet :refer :all]
            [poker.ranges :refer :all]))

(fact
  (let [kq [:CARD_LIST [:RANK "K"] "," [:CARD_LIST [:RANK "Q"]]]
        k  [:CARD_LIST [:RANK "K"]]]
    (set (cards-in-range kq)) => #{"Kh" "Ks" "Kc" "Kd" "Qs" "Qc" "Qh" "Qd"}
    (set (cards-in-range k)) => #{"Kh" "Ks" "Kc" "Kd"}))

(fact
  (let [k-plus [:RANK_PLUS [:RANK "K"]]]
    (set (cards-in-range k-plus)) => #{"Kh" "Ks" "Kc" "Kd" "As" "Ac" "Ah" "Ad"}))

(fact
  (let [deuce-spades [:SUITED_RANK [:RANK "2"] [:SUIT "s"]]]
    (set (cards-in-range deuce-spades)) => #{"2s"}))

(def ranges
  [[:RANDOM]
   [:RANDOM]
   [:RANDOM]
   [:RANDOM]
   [:SUITED_RANK [:RANK "2"] [:SUIT "s"]] 
   [:RANK_PLUS [:RANK "K"]]
   [:CARD_LIST [:RANK "K"] "," [:CARD_LIST [:RANK "Q"]]]])

(defn all-distinct?
  [coll]
  (loop [dist #{}
         coll coll]
    (if-let [el (first coll)]
      (if (dist el)
        false
        (recur (conj dist el) (rest coll)))
      true)))

(fact
  (let [phs (for [r ranges
                  :let [card-range (cards-in-range r)]]
              {:range (set card-range)
               :cardinality (count card-range)})]
    (every? all-distinct? (repeatedly 1000 #(fill-placeholders phs))) => true))
