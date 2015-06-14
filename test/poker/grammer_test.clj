(ns poker.grammer-test
  (:require [midje.sweet :refer :all]
            [poker.grammar :refer :all]))

(fact
  (parser "K+") => [:HAND [:CARD [:RANK_PLUS [:RANK "K"] "+"]]])

(fact
  (class (parser "")) =not=> instaparse.gll.Failure
  (class (parser "K")) =not=> instaparse.gll.Failure
  (class (parser "KT")) =not=> instaparse.gll.Failure
  (class (parser "KsTc")) =not=> instaparse.gll.Failure
  (class (parser "Ks*")) =not=> instaparse.gll.Failure)
