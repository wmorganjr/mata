(ns poker.observe)

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
  (format "<p>Winning hand was a %s.</p>" (name (:hand hand))))

(defn summarize-winners
  [players]
  (format "<p>Winners were player(s) %s</p>" (commaify (map inc players))))

(defn summarize-run
  [{:keys [hands flop winning-hand winning-players]}]
  (apply str (summarize-hands hands)
             (summarize-flop  flop)
             (summarize-win   winning-hand)
             (summarize-winners   winning-players)))
