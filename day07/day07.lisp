#!/usr/bin/env -S sbcl --script

(require "asdf")
(require "split-sequence")

(defvar lines (with-open-file (f "a/input")
                (loop for line = (read-line f nil)
                      while line
                      collect line)))

(defvar crabs (loop for crab in (SPLIT-SEQUENCE:split-sequence #\, (first lines))
                    collect (parse-integer crab)))

(defun cost1 (crabs pos) (loop for crab in crabs sum (abs (- crab pos))))
(defun cost2 (crabs pos) (loop for crab in crabs sum (* (/ (abs (- crab pos)) 2)
                                                        (+ (abs (- crab pos)) 1))))

(defun mincost (crabs costfn &optional best-cost best-pos last-pos)
  (let ((pos (if last-pos (1+ last-pos) (loop for crab in crabs minimize crab))))
    (let ((cost (apply costfn (list crabs pos))))
      (if (> pos (loop for crab in crabs maximize crab))
        (apply costfn (list crabs best-pos))
        (if (and best-cost (< best-cost cost))
          (mincost crabs costfn best-cost best-pos pos)
          (mincost crabs costfn cost      pos      pos))))))

(format t "Part A: ~a~%" (mincost crabs #'cost1))
(format t "Part B: ~a~%" (mincost crabs #'cost2))
