(ns poker.evaluate-test
  (:require [midje.sweet :refer :all]
            [poker.evaluate :refer :all]))

(defn- base2-str [x] (Integer/toString (int x) 2))

(fact deck-basics
  (base2-str (deck "K♦")) => "1000000000000100101100100101"
  (base2-str (deck "5♠")) => "10000001001100000111"
  (base2-str (deck "J♣")) => "10000000001000100100011101"
  (base2-str (deck "Kd")) => "1000000000000100101100100101"
  (base2-str (deck "5s")) => "10000001001100000111"
  (base2-str (deck "Jc")) => "10000000001000100100011101")

(defn hand
  [rank hand-name]
  (fn [result]
    (and (= rank (:rank result)) (= hand-name (:hand result)))))

(defn hand-and-cards
  [rank hand-name best-cards]
  (every-pred (hand rank hand-name)
              #(= best-cards (:cards %))))

(fact evaluation
  (evaluate "T♣" "J♣" "Q♣" "K♣" "A♣") => (hand    1 :StraightFlush)
  (evaluate "A♣" "2♣" "3♣" "4♣" "5♣") => (hand   10 :StraightFlush)
  (evaluate "K♣" "A♣" "A♥" "A♦" "A♣") => (hand   11 :FourOfAKind)
  (evaluate "3♣" "2♣" "2♥" "2♦" "2♣") => (hand  166 :FourOfAKind)
  (evaluate "K♣" "K♥" "A♥" "A♦" "A♣") => (hand  167 :FullHouse)
  (evaluate "3♣" "3♥" "2♥" "2♦" "2♣") => (hand  322 :FullHouse)
  (evaluate "9♣" "J♣" "Q♣" "K♣" "A♣") => (hand  323 :Flush)
  (evaluate "2♣" "3♣" "4♣" "5♣" "7♣") => (hand 1599 :Flush)
  (evaluate "T♣" "J♣" "Q♦" "K♥" "A♣") => (hand 1600 :Straight)
  (evaluate "A♣" "2♣" "3♦" "4♥" "5♣") => (hand 1609 :Straight)
  (evaluate "Q♣" "K♥" "A♥" "A♦" "A♣") => (hand 1610 :ThreeOfAKind)
  (evaluate "4♣" "3♥" "2♥" "2♦" "2♣") => (hand 2467 :ThreeOfAKind)
  (evaluate "Q♣" "K♣" "K♦" "A♥" "A♣") => (hand 2468 :TwoPairs)
  (evaluate "4♣" "3♣" "3♦" "2♥" "2♣") => (hand 3325 :TwoPairs))

(fact evaluation-more-than-5-cards
  (evaluate "8♣" "9♦" "T♣" "J♣" "Q♦" "K♥" "A♣") =>
    (hand-and-cards 1600 :Straight '("T♣" "J♣" "Q♦" "K♥" "A♣"))
  (evaluate "A♣" "K♦" "Q♣" "J♣" "T♦" "9♥" "8♣") =>
    (hand-and-cards 1600 :Straight '("A♣" "K♦" "Q♣" "J♣" "T♦"))
  (evaluate "9♦" "T♣" "J♣" "Q♦" "K♥" "A♣") =>
    (hand-and-cards 1600 :Straight '("T♣" "J♣" "Q♦" "K♥" "A♣")))
