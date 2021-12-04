#!/usr/bin/env -S sbcl --script

(defvar input
  (with-open-file (f "a/input")
    (loop for line = (read-line f nil)
          while line
          collect (parse-integer line))))

(defun increase (numbers)
  (if (cdr numbers)
    (+
      (if
        (< (car numbers) (cadr numbers)) 
        1
        0)
      (increase (rest numbers)))
    0))

(format t "~a ~%" (increase input))

(defun increase-window (numbers)
  (if (>= (list-length numbers) 4)
    (+
      (if
        (< (+ (car numbers) (cadr numbers) (caddr numbers))
           (+ (cadr numbers) (caddr numbers) (cadddr numbers)))
        1
        0)
      (increase-window (rest numbers)))
    0))

(format t "~a ~%" (increase-window input))
