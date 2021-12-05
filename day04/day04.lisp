#!/usr/bin/env -S sbcl --script

(require "uiop")
(require "asdf")
(require "split-sequence")

(defvar draws
  (loop for draw in (uiop:split-string
                      (with-open-file (f "a/input") (read-line f nil))
                      :separator ",")
        collect (parse-integer draw)))

(defvar boards
  (split-sequence:split-sequence
    nil
    (with-open-file (f "a/input")
      (loop for line = (read-line f nil)
            until (string= line ""))
      (loop for line = (read-line f nil)
            while line
            collect 
            (if
              (string= line "")
              nil
              (loop
                for space in (uiop:split-string line :separator " ")
                if (not (string= space ""))
                collect (parse-integer space)))))))

(defun win (board draws)
  (or
    (win-row board draws)
    (win-col board draws)))

(defun win-row (board draws)
  (if board
    (if (set-difference (first board) draws :test #'=)
      (win-row (rest board) draws)
      T)
    NIL))

(assert (win-row (list (list 1 2 3 4 5)) (list 5 4 3 2 1)))
(assert (not (win-row (list (list 1 2 3 4 5)) (list 5 4 3 2))))

(defun col (board i)
  (if board
    (cons (elt (first board) i) (col (rest board) i))
    nil))

(defun win-col (board draws)
  (win-row
    (loop for i from 0 below (length (first board))
          collect (col board i))
    draws))

(assert (win-col (list '(1) '(2) '(3) '(4) '(5)) '(5 4 3 2 1)))

(defun first-win (boards draws to-draw)
  (if to-draw
    (or
      (first-win boards (cons (first to-draw) draws) NIL)
      (first-win boards (cons (first to-draw) draws) (rest to-draw)))
    (if boards
      (if (win (first boards) draws)
        (list draws (first boards))
        (first-win (rest boards) draws to-draw))
      NIL)))

(defun score (board draws)
  (loop for square in (loop for row in board append (set-difference row draws)) sum square))

(defvar first-winning-board (first-win boards nil draws))
(defvar first-board-value (score (cadr first-winning-board) (car first-winning-board)))
(defvar winning-draw (caar first-winning-board))

(format t "~a x ~a = ~a~%" first-board-value winning-draw (* first-board-value winning-draw))

; return the board that wins when there is one board left
(defun last-win (boards draws to-draw)
  (let
    ((draws   (cons (car to-draw) draws)) ; draw a number
     (to-draw (cdr to-draw)))             ; from to-draw
    (if (rest boards)
      (last-win 
        (loop for board in boards unless (win board draws) collect board)
        draws
        to-draw)
      (if (win (first boards) draws)
        (list draws (first boards))
        (last-win boards draws to-draw)))))

(defvar last-winning-board (last-win boards nil draws))
(defvar last-board-value (score (cadr last-winning-board) (car last-winning-board)))
(defvar last-winning-draw (caar last-winning-board))

(format t "~a x ~a = ~a~%" last-board-value last-winning-draw (* last-board-value last-winning-draw))
