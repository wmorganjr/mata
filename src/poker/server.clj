(ns poker.server
  (:require [compojure.core :refer :all]
            [poker.ranges :as ranges]
            [poker.mata :as mata]
            [poker.grammar :as grammar]
            [poker.evaluate :as evaluate]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [instaparse.core :as insta]
            [compojure.route :as route]))

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

;(defn run-equity
;  [req]
;  {:status 200
;   :headers {}
;   :body   (str (frequencies (map :winning-players (runner 100))))})
;
;(defn run-tests
;  [req]
;  {:status 200
;   :headers {}
;   :body   (summarize-run (first (runner 100)))})

(def card-params
  ["player-1"
   "player-2"
   "player-3"
   "player-4"
   "flop"
   "dead"])

(defn parse-params
  [query-params]
  (into {}
    (for [[k v] (select-keys query-params card-params)
          :when (seq v)]
      [k (grammar/parser v)])))

(def random-card
  [:CARD [:RANDOM "*"]])

(defn complete-with-random
  [spec]
  (-> spec
    (update-in ["flop"] #(concat % (repeat (inc (- 3 (count %))) random-card)))
    (update-in ["player-1"] #(if (seq %)
                               (concat % (repeat (inc (- 5 (count %))) random-card))))
    (update-in ["player-2"] #(if (seq %)
                               (concat % (repeat (inc (- 5 (count %))) random-card))))
    (update-in ["player-3"] #(if (seq %)
                               (concat % (repeat (inc (- 5 (count %))) random-card))))
    (update-in ["player-4"] #(if (seq %)
                               (concat % (repeat (inc (- 5 (count %))) random-card))))))

(defn monte-carlo
  [placeholders n]
  (for [trial (repeatedly n #(ranges/fill-placeholders placeholders))
        :let [groups (group-by :type trial)
              result (mata/runner {"player-1" (map :card (groups "player-1"))
                                   "player-2" (map :card (groups "player-2"))
                                   "player-3" (map :card (groups "player-3"))
                                   "player-4" (map :card (groups "player-4"))
                                   "flop"     (map :card (groups "flop"))})]]
    result))

(defn summarize-mc
  [trials n]
  (let [start (System/currentTimeMillis)
        freqs (into {}
                (for [[k v] (frequencies (map :winning-players trials))]
                  [(vec k) v]))
        wins  (apply merge-with + (for [[k v] freqs
                                        kk k]
                                    {kk v}))
        chops (count (filter #(= :Chop (:hand (:winning-hand %))) trials))
        tot   (apply + (vals wins))
        equity (into {} (for [[k v] wins]
                          [(str "player-" (inc k)) (format "%.2f" (double (/ v tot)))]))]
        
    {:winners (for [[k v] freqs]
                {:players k
                 :count   v})
     :chop    (format "%.2f" (double (/ chops tot)))
     :equity  equity
     :trials  n
     :time    (- (System/currentTimeMillis) start)}))

(defn run-big
  [req]
  (let [x (-> (:query-params req)
              (parse-params)
              (complete-with-random)
              (ranges/spec->phs)
              (monte-carlo 1000)
              (summarize-mc 1000))]
    {:status 200
     :body x}))

(defn run-one
  [req]
  (let [x (-> (:query-params req)
              (parse-params)
              (complete-with-random)
              (ranges/spec->phs)
              (monte-carlo 1))]
    {:status 200
     :body {:resp x}}))

(defroutes poker-routes
  (GET "/" [] "<h1>Hello World</h1>")
  (GET "/something" [] "<h1>Hello Something</h1>")
;  (GET "/tests" [] run-tests)
  (GET "/big" [] run-big)
  (GET "/one" [] run-one)
;  (GET "/equity" [] run-equity)
  (route/not-found "<h1>Page not found</h1>"))

;player-1=AsKc&flop=567&player-2=66

(def app
  (-> poker-routes
      (wrap-json-response)
      (wrap-params)
      (wrap-resource "public")))



