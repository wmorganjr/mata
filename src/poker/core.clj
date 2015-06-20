(ns poker.core)

(def ranks '[A K Q J T 9 8 7 6 5 4 3 2])
(def suits '[s d h c])

(def cards
  (set (for [rank ranks
             suit suits]
         (str rank suit))))

(def card->num
  (let [cards (vec (for [rank ranks
                         suit suits]
                     (str rank suit)))]
    (fn [card]
      (.indexOf cards card))))
