(defproject poker "0.1.0-SNAPSHOT"
  :description "Aaron Been Poker Evaluator"
  :plugins [[lein-ring "0.9.5"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [instaparse "1.4.0"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [ring/ring-json "0.3.1"]
                 [compojure "1.3.4"]]
  :ring {:handler poker.server/app}
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}})
