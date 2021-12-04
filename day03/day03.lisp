#!/usr/bin/env -S sbcl --script

(defvar input (with-open-file (f "a/input")
                (loop for line = (read-line f nil)
                      while line
                      collect line)))

(defvar width (length (first input)))

(defun ones (input pos)
  (loop for entry in input
        when (string= (elt entry pos) "1")
        sum 1))

(defvar gamma (parse-integer
                (format nil "~{~a~}"
                        (loop for i from 0 below width
                              collect (if (> (ones input i) (/ (length input) 2)) 1 0)))
                :radix 2))

(defvar epsilon (parse-integer
                  (format nil "~{~a~}"
                          (loop for i from 0 below width
                                collect (if (< (ones input i) (/ (length input) 2)) 1 0)))
                  :radix 2))

(format t "~a x ~a = ~a~%" gamma epsilon (* gamma epsilon))

(defun winnow (items pos cmp)
  (if (or (= (length items) 1)
          (>= pos (length (car items))))
    (car items)
    (winnow
      (let ((target (if (funcall cmp (ones items pos) (/ (length items) 2)) "1" "0")))
        (loop for item in items
              when (string= (elt item pos) target)
              collect item))
      (1+ pos)
      cmp)))

(defvar oxygen (parse-integer (winnow input 0 #'>=) :radix 2))
(defvar co2 (parse-integer (winnow input 0 #'<)  :radix 2))

(format t "~a x ~a = ~a~%" oxygen co2 (* oxygen co2))

