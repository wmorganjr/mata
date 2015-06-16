(ns poker.server
  (:require [compojure.core :refer :all]
            [poker.observe :as observe]
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
  (pmap (fn [trial]
          (let [groups (group-by :type trial)
                result (mata/runner {"player-1" (map :card (groups "player-1"))
                                     "player-2" (map :card (groups "player-2"))
                                     "player-3" (map :card (groups "player-3"))
                                     "player-4" (map :card (groups "player-4"))
                                     "flop"     (map :card (groups "flop"))})]
            result))
        (repeatedly n #(ranges/fill-placeholders placeholders))))

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
                          [(str "player-" (inc k)) (format "%.3f" (double (/ v tot)))]))
        win    (into {} (for [[k v] freqs
                              :when (= 1 (count k))]
                          [(str "player-" (inc (first k))) (format "%.3f" (double (/ v n)))]))]
        
    {:winners (for [[k v] freqs]
                {:players k
                 :count   v})
     :win     win
     :chop    (format "%.3f" (double (/ chops tot)))
     :equity  equity
     :trials  n
     :time    (- (System/currentTimeMillis) start)}))

(defn run-big
  [req]
  (prn "REQ" (assoc req :time (str (org.joda.time.DateTime/now))))
  (try
    (let [trials (Long/parseLong (get (:query-params req) "trials"))
          x (-> (:query-params req)
                (parse-params)
                (complete-with-random)
                (ranges/spec->phs)
                (monte-carlo trials)
                (summarize-mc trials))]
      (prn "RESP" x)
      {:status 200
       :body x})
    (catch Exception e
      (let [time (System/currentTimeMillis)]
        (println time)
        (.printStackTrace e)
        {:status 200
         :body   {:error time}}))))

(defn run-one
  [req]
  (let [x (-> (:query-params req)
              (parse-params)
              (complete-with-random)
              (ranges/spec->phs)
              (monte-carlo 1))]
    {:status 200
     :body {:resp x}}))

(defn run-summary
  [req]
  (let [x (-> (:query-params req)
              (parse-params)
              (complete-with-random)
              (ranges/spec->phs)
              (monte-carlo 1)
              (first))]
    {:status 200
     :body (observe/summarize-run x)}))

(defroutes poker-routes
  (GET "/" [] "<a href='calc.html'>Calc</a>")
  (GET "/big" [] run-big)
  (GET "/one" [] run-one)
  (GET "/singleHand.html" [] run-summary)
  (route/not-found "<h1>Page not found</h1>"))

(def app
  (-> poker-routes
      (wrap-json-response)
      (wrap-params)
      (wrap-resource "public")))
