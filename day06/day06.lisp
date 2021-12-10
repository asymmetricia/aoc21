#!/usr/bin/env -S sbcl --script

(require "asdf")
(require "split-sequence")

(defvar lines (with-open-file (f "a/input")
                (loop for line = (read-line f nil)
                      while line
                      collect line)))

(defvar fish (loop for fish in (SPLIT-SEQUENCE:split-sequence #\, (first lines))
                   collect (parse-integer fish)))

(defvar schools (list 0 0 0 0 0 0 0 0 0))
(loop for fish in fish
        do (setf (elt schools fish) (1+ (elt schools fish))))

(loop for day from 1 to 80
      do (setf schools (list
                         (elt schools 1) ; 0
                         (elt schools 2) ; 1
                         (elt schools 3) ; 2
                         (elt schools 4) ; 3
                         (elt schools 5) ; 4
                         (elt schools 6) ; 5
                         (+ (elt schools 7) (elt schools 0)) ; 6
                         (elt schools 8) ; 7
                         (elt schools 0))))

(format t "Day  80: ~a~%" (loop for school in schools sum school))

(loop for day from 81 to 256
      do (setf schools (list
                         (elt schools 1) ; 0
                         (elt schools 2) ; 1
                         (elt schools 3) ; 2
                         (elt schools 4) ; 3
                         (elt schools 5) ; 4
                         (elt schools 6) ; 5
                         (+ (elt schools 7) (elt schools 0)) ; 6
                         (elt schools 8) ; 7
                         (elt schools 0))))

(format t "Day 256: ~a~%" (loop for school in schools sum school))
