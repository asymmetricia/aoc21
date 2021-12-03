#!/usr/bin/env -S sbcl --script

(require "uiop")

(defvar input 
  (with-open-file (f "a/input")
    (loop for line = (read-line f nil)
          while line
          collect (uiop:split-string line :separator " "))))

(defun horiz (input)
  (loop for command in input
        if (string= (car command) "forward")
        sum (parse-integer (cadr command))))

(defun depth (input)
  (-
    (loop for command in input
          if (string= (car command) "down")
          sum (parse-integer (cadr command)))
    (loop for command in input
          if (string= (car command) "up")
          sum (parse-integer (cadr command)))))

(let
  ((depth (depth input))
   (horiz (horiz input)))
  (format t "~a x ~a = ~a ~%" depth horiz (* depth horiz)))

(defun aimdepth (input &optional aim)
  (if input
    (let
      ((aim (or aim 0))
       (cmd (caar input))
       (amt (parse-integer (cadar input))))
      (if (string= cmd "forward")
        (+ (* aim amt) (aimdepth (rest input) aim))
        (if (string= cmd "up")
          (aimdepth (rest input) (- aim amt))
          (aimdepth (rest input) (+ aim amt)))))
    0))

(let
  ((depth (aimdepth input))
   (horiz (horiz input)))
  (format t "~a x ~a = ~a ~%" depth horiz (* depth horiz)))
