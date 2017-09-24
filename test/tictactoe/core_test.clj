(ns tictactoe.core-test
  (:require [clojure.test :refer :all]
            [tictactoe.core :refer :all]))

(deftest test-game
  (let [{:keys [board result]} (-> NEW-GAME
                                   (move 1 1)
                                   (move 1 2)
                                   (move 2 2)
                                   (move 0 1)
                                   (move 0 0))
        winning-combo (nwse-combo board)]
    (is (= [:x :x :x] winning-combo))
    (is (= :x (winner winning-combo)))
    (is (= :x result))))
