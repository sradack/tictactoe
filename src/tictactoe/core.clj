(ns tictactoe.core
  (:require [clojure.string :as str])
  (:gen-class))

(def EMPTY-BOARD
  [[nil nil nil]
   [nil nil nil]
   [nil nil nil]])

(def NEW-GAME
  {:board EMPTY-BOARD
   :player :x
   :eval 0})

(def opposite-player
  {:x :o
   :o :x})

(def optimize {:x > :o <})
(def worst-eval {:x -1 :o 1})

(def symbol->char {nil "." :x "X" :o "O"})

(defn row-combo
  "The values of a given row."
  [board row]
  (for [col (range 3)]
    (get-in board [row col])))

(defn col-combo
  "The values of a given column."
  [board col]
  (for [row (range 3)]
    (get-in board [row col])))

(defn nwse-combo
  "The values of the board diagonally from northwest to southeast."
  [board]
  [(get-in board [0 0])
   (get-in board [1 1])
   (get-in board [2 2])])

(defn swne-combo
  "The values of the board diagonally from southwest to northest."
  [board]
  [(get-in board [0 2])
   (get-in board [1 1])
   (get-in board [2 0])])

(defn winner
  "Determines the winner of a combination, nil if no winner."
  [s]
  (when (apply = s)
    (first s)))

(defn result
  "Determines the result of the game given a board, nil if undecided."
  [board]
  (or (some winner
            (list
              (col-combo board 0)
              (col-combo board 1)
              (col-combo board 2)
              (row-combo board 0)
              (row-combo board 1)
              (row-combo board 2)
              (nwse-combo board)
              (swne-combo board)))
      (when (not-any? nil? (flatten board))
        :cats)))

(defn position->row-col
  "Maps a position number (as on a numpad) to [row col] for the board."
  [position]
  [(- 2 (quot (dec position) 3)) (mod (dec position) 3)])

(defn move
  "Given a move, returns the next game state."
  ([game position]
   (apply move game (position->row-col position)))
  ([{:keys [board player] :as game} row col]
   (let [board' (assoc-in board [row col] player)]
     (-> game
         (assoc :board board')
         (assoc :player (opposite-player player))
         (assoc :result (result board'))))))

(defn valid-moves
  "Returns a sequence of valid next game states."
  [{:keys [board result] :as game}]
  (when-not result
    (apply concat
           (for [row (range 3)]
             (filter #(not (nil? %))
                     (for [col (range 3)]
                       (let [e (get-in board [row col])]
                         (when-not e
                           (move game row col)))))))))

(defn evaluate
  "Evaluates the board, scoring between [-1,1]"
  [{:keys [result]}]
  (cond (= result :x) 1.0
        (= result :o) -1.0
        :else 0))

(defn minimax
  "Use minimax with pruning to find the best move."
  ([game]
   (minimax game 9 nil))
  ([game depth]
   (minimax game depth nil))
  ([{:keys [player result] :as game} depth must-beat-eval]
   (let [better (-> player optimize)]
     (if (or result (zero? depth))
       (assoc game :eval (evaluate game))
       (let [best-move (reduce
                         (fn [best-move-so-far move]
                           (if (or (not best-move-so-far)
                                   (not must-beat-eval)
                                   (better must-beat-eval (:eval best-move-so-far)))
                             (let [move (minimax move (dec depth) (:eval best-move-so-far))]
                               (if (or (not best-move-so-far)
                                       (better (:eval move) (:eval best-move-so-far)))
                                 move
                                 best-move-so-far))
                             (reduced best-move-so-far)))
                         nil
                         (valid-moves game))]
         (assoc game
                :eval (:eval best-move)
                :best-move best-move))))))

(defn board->string
  "Generates a string representation of the board."
  [board]
  (str/join "\n-+-+-\n" (map (fn [row] (str/join "|" (map symbol->char row))) board)))

(defn render
  "Prints out the game state."
  [{:keys [board player eval result]}]
  (println "=====")
  (println (board->string board))
  (println "=====")
  (println "Evaluation: " eval)
  (println "Next Player:" (symbol->char player))
  (println "====="))

(defn -main
  "Plays a game of Tic-Tac-Toe."
  [& args]
  (loop [{:keys [result] :as game} NEW-GAME]
    (render game)
    (if result
      (println (case result
                 :x "X wins!"
                 :o "O wins!"
                 :cats "Cats!"))
      (do
        (println "Enter a number using numpad (1-9) number or 'b' for the best move: ")
        (let [input (-> (read-line) str/trim read-string)]
          (cond (= 'b input) (recur (-> game (minimax) :best-move))
                (number? input) (recur (-> game (move input) (minimax)))
                :else (println "Got '" input "'.  Exiting...")))))))
