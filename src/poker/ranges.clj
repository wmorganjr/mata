(ns poker.ranges
  (:require [clojure.math.combinatorics :as combo]))

(def ranks (map str '[A K Q J T 9 8 7 6 5 4 3 2]))
(def suits (map str '[s d h c]))

(def cards
  (set (for [rank ranks
             suit suits]
         (str rank suit))))

(defmulti cards-in-range first)

(defmethod cards-in-range :RANK_LIST
  [[_ [_ rank] _ rank-rest]]
  (concat (filter #(.startsWith % rank) cards)
          (and rank-rest (cards-in-range rank-rest))))

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
  (let [deck (atom (vec (shuffle cards)))
        phs  (sort-by :cardinality placeholders)]
    (for [ph phs]
      (let [index (first (keep-indexed (fn [index item]
                                         (if (get (:range ph) item)
                                           index))
                                       @deck))
            card  (@deck index)]
        (swap! deck #(assoc % index nil))
        (assoc ph
               :card card)))))

(defn spec->phs
  [completed-spec]
  (for [[k v] completed-spec
        card-spec (rest v)
        :let [cc (cards-in-range (second card-spec))]]
    {:type k
     :range (set cc)
     :cardinality (count cc)}))

;(def counter (atom 0))
;
;(require 'poker.evaluate)
;
;(time
;(let [phs (repeat 6 {:range (set (cards-in-range [:RANDOM]))
;                     :cardinality 52})]
;  (doseq [hand (take 1000 (repeatedly #(fill-placeholders phs)))
;          :let [start (java.lang.System/currentTimeMillis)]]
;    (apply poker.evaluate/evaluate hand)
;    (swap! counter #(+ % (- (java.lang.System/currentTimeMillis) start)))))
;)





