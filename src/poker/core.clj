(ns poker.core
  (:require [compojure.core :refer :all]
            [poker.evaluate :as evaluate]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [instaparse.core :as insta]
            [compojure.route :as route]))

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

(def hand
  (insta/parser
    grammar))

(defn fill-in-random
  [hands flop dead]
  (let [hand-counts (map #(- 5 (count %)) hands)
        flop-count  (- 3 (count flop))
        rand-count  (apply + flop-count hand-counts)
        deck        (reduce disj cards (apply concat flop dead hands))]
    (repeatedly
      (fn []
        (let [cards (atom (take rand-count (shuffle deck)))]
          {:flop (let [fs (take flop-count @cards)]
                   (swap! cards #(drop flop-count %))
                   (concat flop fs))
           :hands (for [[hand c] (map vector hands hand-counts)]
                    (let [hs (take c @cards)]
                      (swap! cards #(drop c %))
                      (concat hand hs)))})))))

(def ev (atom 0))

(defn best-hand
  [hand flop]
  (let [best (apply min-key :rank (for [f flop]
                                    (let [start (System/currentTimeMillis)
                                          e     (apply evaluate/evaluate (conj hand f))]
                                      (swap! ev #(+ % (- (System/currentTimeMillis) start)))
                                      e)))]
    (if (< 3545 (:rank best))
      {:rank 3546
       :hand :Chop}
      best)))

(defn runner
  [n]
  (for [{:keys [hands flop]} (take n (fill-in-random [[][]] [] []))]
    (let [bests  (map #(best-hand % flop) hands)
          winner (apply min-key :rank bests)]
      {:hands   hands
       :flop    flop
       :winning-hand winner
       :winning-players (keep-indexed #(if (= (:rank %2) (:rank winner)) %1) bests)})))



