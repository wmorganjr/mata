(ns poker.runout
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.java.io :as io]
            [poker.grammar :as grammar]
            [poker.mata :as mata]
            [poker.core :as poker]))

(defn trial
  [my-cards his-cards flop]

  ; The deck has all the cards that aren't in my
  ; hand, his hand, or on the flop.
  (let [deck      (sort (reduce disj (set poker/cards)
                                (concat my-cards his-cards flop)))

        ; The run outs are every way of taking 4 cards
        ; from the deck.
        run-outs  (combinations deck 4)]

    ; For each of the run-outs, there are six ways to split the
    ; cards between me and him.
    (for [run-out  run-outs
          my-draws (combinations run-out 2)
          :let [his-draws (remove (set my-draws) run-out)]]

      ; Run the rules of Mata Aces on this run out.
      (mata/runner
        {"player-1" (concat my-cards my-draws)
         "player-2" (concat his-cards his-draws)
         "flop"     flop}))))

(defn description
  [my-cards his-cards flop]
  (let [my-cards  (sort-by poker/card->num my-cards)
        his-cards (sort-by poker/card->num his-cards)
        flop      (sort-by poker/card->num flop)]
    {:p1-cards (apply str my-cards)
     :p2-cards (apply str his-cards)
     :flop     (apply str flop)
     :id       (apply str (concat my-cards his-cards flop))}))

(def trials
  (atom (into {} (for [file (.listFiles (io/file "resources/runouts"))
                       :let [data (read-string (slurp file))]]
                   [(.getName file) {:p1-cards (:p1-cards data)
                                     :p2-cards (:p2-cards data)
                                     :flop     (:flop data)
                                     :id       (:id data)
                                     :result (future (:result data))
                                     :start  (:start data)
                                     :end    (future (:end data))
                                     :numerator (future 740460)
                                     :denominator 740460}]))))

(defn save-result!
  [desc start result]
  (->> {:result result
        :start  start
        :end    (System/currentTimeMillis)}
       (merge desc)
       (prn-str)
       (spit (io/file "resources/runouts" (:id desc))))
  result)

(defn load-results!
  []
  (doseq [file (.listFiles (io/file "resources/runouts"))]
    (swap! trials
      #(assoc % (.getName file (read-string (slurp file)))))))

(defn start-trial
  [my-cards his-cards flop]
  (let [desc      (description my-cards his-cards flop)]
    (if-not (@trials (:id desc))
      (let [numerator (atom 0)
            start     (System/currentTimeMillis)
            end       (atom nil)
            result    (future
                        (->> (trial my-cards his-cards flop)
                             (map #(do (swap! numerator inc) %))
                             (map :winning-players)
                             (frequencies)
                             (save-result! desc start)
                             (#(do (reset! end (System/currentTimeMillis)) %))))
            trial     (assoc desc
                        :denominator 740460
                        :numerator   numerator
                        :result      result
                        :start       (System/currentTimeMillis)
                        :end         end)]
        
        (swap! trials #(assoc % (:id trial) trial))
        {:message (format "Trial launched: %s" (:id trial))})
    {:message "This trial has already been launched."})))

(defn status
  [trial]
  (if (future-done? (:result trial))
    :done
    :running))

(defn format-pct
  [n d]
  (format "%.3f" (double (/ n d))))

(defn describe-results
  [results]
  (let [trials (apply + (vals results))
        chop   (results [0 1] 0)
        p1     (results [0] 0)
        p2     (results [1] 0)]
    {:winpct {:player_1 (format-pct p1   trials)
              :player_2 (format-pct p2   trials)
              :chop     (format-pct chop trials)}
     :equity {:player_1 (format-pct (+ p1 chop) (+ trials chop))
              :player_2 (format-pct (+ p2 chop) (+ trials chop))}
     :wins   {:player_1 p1
              :player_2 p2
              :chop     chop}}))

(def empty-result
  {:winpct {:player_1 ""
            :player_2 ""
            :chop     ""}
   :equity {:player_1 ""
            :player_2 ""}
   :wins   {:player_1 0
            :player_2 0
            :chop     0}})

(defn describe-trial
  [trial]
  (let [trial-status (status trial)]
    (-> trial
      (update-in [:numerator] deref)
      (update-in [:end] deref)
      (update-in [:result] (if (= :done trial-status)
                             (comp describe-results deref)
                             (constantly empty-result)))
      (assoc :status trial-status))))

(defn describe-trials
  [& _]
  {:status 200
   :body {:data (map describe-trial (sort-by :start (vals @trials)))}})

(defn parse-three-cards
  [cards]
  (map #(apply str %) (partition 2 (grammar/normalize-case cards))))

(defn invalid-card
  [cards]
  (first (filter #(= -1 (poker/card->num %)) cards)))

(defn problem
  [cards]
  (if-let [card (invalid-card cards)]
    (format "This is not a valid card: %s" card)
    (if (not= 9 (count cards))
      "You must submit a runout with 9 cards exactly"
      (if (not= 9 (count (set cards)))
        "It looks like a card has been repeated"))))

(def exception-fmt
  "There was an error on the server. Try a different query and see if that fixes it. Otherwise, contact Will with this timestamp: %s")

(defn post
  [req]
  (prn "RU-REQ" (assoc req :time (str (org.joda.time.DateTime/now))))
  {:body (try
           (let [{:strs [my-cards his-cards flop]} (:form-params req)
                 my-cards  (parse-three-cards my-cards)
                 his-cards (parse-three-cards his-cards)
                 flop      (parse-three-cards flop)
                 prob      (problem (concat my-cards his-cards flop))]
             (if prob
               {:message prob}
               (start-trial my-cards
                            his-cards
                            flop)))
           (catch Exception e
             (let [time (System/currentTimeMillis)]
               (println time)
               (.printStackTrace e)
               {:message (format exception-fmt time)})))})
