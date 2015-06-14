(ns poker.mata-test
  (:require [midje.sweet :refer :all]
            [poker.mata :refer :all]))

(fact
  (:hand (best-hand ["5c" "3d" "4s" "2c" "As"] ["4h" "7h" "3d"])) => :Straight
  (:hand (best-hand ["Jh" "3h" "4s" "2h" "Ah"] ["4h" "7h" "3d"])) => :Flush
  (:hand (best-hand ["5c" "5d" "4s" "Ac" "As"] ["4h" "7h" "3d"])) => :TwoPairs
  (:hand (best-hand ["Ac" "Ad" "4s" "2c" "As"] ["4h" "7h" "3d"])) => :FullHouse
  (:hand (best-hand ["5c" "4d" "3s" "Jc" "Qs"] ["4h" "7h" "3d"])) => :Chop)