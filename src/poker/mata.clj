(ns poker.mata
  (:require [poker.evaluate :as evaluate]))

(defn best-hand
  [hand flop]
  (if (seq hand)
    (let [best (->> (for [card flop]
                      (apply evaluate/evaluate (conj hand card)))
                    (apply min-key :rank))]
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
