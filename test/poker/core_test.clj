(ns poker.core-test
  (:require [midje.sweet :refer :all]
            [poker.core :refer :all]))

(fact 1 => 1)

(fact (with-out-str (foo "bar")) => "bar Hello, World!\n")

(fact
  (class (hand "")) =not=> instaparse.gll.Failure
  (class (hand "K")) =not=> instaparse.gll.Failure
  (class (hand "KT")) =not=> instaparse.gll.Failure
  (class (hand "KsTc")) =not=> instaparse.gll.Failure
  (class (hand "Ks*")) =not=> instaparse.gll.Failure)

(fact
  (:hand (best-hand ["5c" "3d" "4s" "2c" "As"] ["4h" "7h" "3d"])) => :Straight
  (:hand (best-hand ["Jh" "3h" "4s" "2h" "Ah"] ["4h" "7h" "3d"])) => :Flush
  (:hand (best-hand ["5c" "5d" "4s" "Ac" "As"] ["4h" "7h" "3d"])) => :TwoPairs
  (:hand (best-hand ["Ac" "Ad" "4s" "2c" "As"] ["4h" "7h" "3d"])) => :FullHouse
  (:hand (best-hand ["5c" "4d" "3s" "Jc" "Qs"] ["4h" "7h" "3d"])) => :Chop)



