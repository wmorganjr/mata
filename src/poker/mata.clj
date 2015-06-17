(ns poker.mata
  (:require [poker.evaluate :as evaluate]))

(defn eval-mata
  [hand flop]
  (let [rank (apply min
               (map evaluate/calculate-hand-rank
                 (conj
                   (let [handv (vec hand)]
                     (for [i (range 5)
                           f flop]
                       (assoc handv i f)))
                   hand)))]
    {:rank rank :hand (evaluate/resolve-rank-name rank)}))

(defn best-hand
  [hand flop]
  (if (seq hand)
    (let [best (eval-mata hand flop)]
      (if (< 3545 (:rank best))
        {:rank 3546
         :hand :Chop}
        best))
    {:rank 3547
     :hand :Out}))

(defn runner
  [trial]
  (let [{:strs [player-1 player-2 player-3 player-4 flop]} trial]
    (let [hands  [player-1 player-2 player-3 player-4]
          bests  (map #(best-hand % flop) hands)
          winner (apply min-key :rank bests)]
      {:hands   hands
       :flop    flop
       :winning-hand winner
       :winning-players (keep-indexed #(if (= (:rank %2) (:rank winner)) %1) bests)})))
