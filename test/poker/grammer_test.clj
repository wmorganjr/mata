(ns poker.grammer-test
  (:require [midje.sweet :refer :all]
            [poker.grammar :refer :all]))

(fact
  (parser "K+") => [:HAND [:CARD [:CARD_LIST [:RANK_PLUS [:RANK "K"] "+"]]]]
  (parser "k+") => [:HAND [:CARD [:CARD_LIST [:RANK_PLUS [:RANK "K"] "+"]]]]

  (parser "5s") => [:HAND [:CARD [:CARD_LIST [:SUITED_RANK [:RANK "5"] [:SUIT "s"]]]]]
  (parser "tD") => [:HAND [:CARD [:CARD_LIST [:SUITED_RANK [:RANK "T"] [:SUIT "d"]]]]]

  (parser "T,9,8") => [:HAND [:CARD [:CARD_LIST [:RANK "T"] "," [:CARD_LIST [:RANK "9"] "," [:CARD_LIST [:RANK "8"]]]]]]
  (parser "Ts7c9s,Js,Qs,Ks,A,T") => [:HAND [:CARD [:CARD_LIST [:SUITED_RANK [:RANK "T"] [:SUIT "s"]]]] [:CARD [:CARD_LIST [:SUITED_RANK [:RANK "7"] [:SUIT "c"]]]] [:CARD [:CARD_LIST [:SUITED_RANK [:RANK "9"] [:SUIT "s"]] "," [:CARD_LIST [:SUITED_RANK [:RANK "J"] [:SUIT "s"]] "," [:CARD_LIST [:SUITED_RANK [:RANK "Q"] [:SUIT "s"]] "," [:CARD_LIST [:SUITED_RANK [:RANK "K"] [:SUIT "s"]] "," [:CARD_LIST [:RANK "A"] "," [:CARD_LIST [:RANK "T"]]]]]]]]]
  (parser "*") => [:HAND [:CARD [:RANDOM "*"]]]

  (parser "Jc3c(3,J)") => [:HAND [:CARD [:CARD_LIST [:SUITED_RANK [:RANK "J"] [:SUIT "c"]]]] [:CARD [:CARD_LIST [:SUITED_RANK [:RANK "3"] [:SUIT "c"]]]] [:CARD [:PAREN_CARD "(" [:CARD [:CARD_LIST [:RANK "3"] "," [:CARD_LIST [:RANK "J"]]]] ")"]]]

  (parser "*s") => [:HAND [:CARD [:CARD_LIST [:ANY_SUIT "*" [:SUIT "s"]]]]]
  (parser "*H") => [:HAND [:CARD [:CARD_LIST [:ANY_SUIT "*" [:SUIT "h"]]]]])

(fact
  (class (parser "")) =not=> instaparse.gll.Failure
  (class (parser "K")) =not=> instaparse.gll.Failure
  (class (parser "KT")) =not=> instaparse.gll.Failure
  (class (parser "KsTc")) =not=> instaparse.gll.Failure
  (class (parser "Jc3c(3,J)")) =not=> instaparse.gll.Failure
  (class (parser "*s")) =not=> instaparse.gll.Failure
  (class (parser "*H")) =not=> instaparse.gll.Failure
  (class (parser "Ks*")) =not=> instaparse.gll.Failure)
