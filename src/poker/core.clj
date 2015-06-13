(ns poker.core
  (:require [compojure.core :refer :all]
            [poker.evaluate :as evaluate]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [instaparse.core :as insta]
            [compojure.route :as route]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

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
           CARD = SUITEDRANK | RANK | RANDOM
           SUITEDRANK = RANK SUIT
           RANK = %s
           SUIT = %s
           RANDOM = '*'"

          ranks-grammar
          suits-grammar))

(def hand
  (insta/parser
    grammar))

(def setup
  {:player-1 [:As :Ac]
   :player-2 [:Ks :Kc]})

(defn random-cards-seqs
  [n dead]
  (take n (shuffle (reduce disj cards dead))))

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

(defn commaify
  [coll]
  (apply str (interpose \, coll)))

(defn summarize-hands
  [hands]
  (apply str
    (for [[i hand] (map vector (range) hands)]
      (format "<p><b>Player %s:</b>%s</p>" (inc i) (commaify hand)))))

(defn summarize-flop
  [flop]
  (format "<p><b>Flop:</b>%s</p>" (commaify flop)))

(defn summarize-win
  [hand]
  (str (format "<p>Winning hand was a %s.</p>" (name (:hand hand)))
       (format "<p>Winning cards were %s.</p>" (commaify (:cards hand)))))

(defn summarize-winners
  [players]
  (format "<p>Winners were player(s) %s</p>" (commaify (map inc players))))

(defn summarize-run
  [{:keys [hands flop winning-hand winning-players]}]
  (apply str (summarize-hands hands)
             (summarize-flop  flop)
             (summarize-win   winning-hand)
             (summarize-winners   winning-players)))

(defn run-equity
  [req]
  {:status 200
   :headers {}
   :body   (str (frequencies (map :winning-players (runner 100))))})

(defn run-tests
  [req]
  {:status 200
   :headers {}
   :body   (summarize-run (first (runner 100)))})

(defn run-atest
  [req]
  {:status 200
   :body {:foo :bar}})

(defroutes poker-routes
  (GET "/" [] "<h1>Hello World</h1>")
  (GET "/something" [] "<h1>Hello Something</h1>")
  (GET "/tests" [] run-tests)
  (GET "/atest" [] run-atest)
  (GET "/equity" [] run-equity)
  (route/not-found "<h1>Page not found</h1>"))

(def app
  (-> poker-routes
      (wrap-json-response)
      (wrap-resource "public")))


