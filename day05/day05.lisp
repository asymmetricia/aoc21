#!/usr/bin/env -S sbcl --script

(defstruct line (ax) (ay) (bx) (by))

(defun line-from-string (string)
  (make-line
    :ax (parse-integer (subseq string 0 (position #\, string)))
    :ay (parse-integer (subseq string (1+ (position #\, string)) (position #\- string)))
    :bx (parse-integer (subseq string (1+ (position #\> string)) (position #\, string :start (position #\> string))))
    :by (parse-integer (subseq string (1+ (position #\, string :start (position #\> string)))))))

(defun line-slope (line)
  (list
    (if (< (line-ax line) (line-bx line))
      1
      (if (> (line-ax line) (line-bx line))
        -1 0))
    (if (< (line-ay line) (line-by line))
      1
      (if (> (line-ay line) (line-by line))
        -1 0))))

(assert (equal (line-slope (make-line :ax 0 :ay 0 :bx 2 :by 2)) (list 1 1)))

(defun mark-line (line target-map)
  (loop with slope = (line-slope line)
        with pt = (list (line-ax line) (line-ay line))
        until (and
                (equal (+ (line-bx line) (car  slope)) (car  pt))
                (equal (+ (line-by line) (cadr slope)) (cadr pt)))
        do (setf (aref target-map (car pt) (cadr pt)) (1+ (aref target-map (car pt) (cadr pt))))
        do (setq pt (list (+ (car pt) (car slope)) (+ (cadr pt) (cadr slope))))))

(defvar input (with-open-file (f "a/input")
                (loop for line = (read-line f nil)
                      while line
                      collect (line-from-string line))))

(defvar minx (loop for line in input minimize (min (line-ax line) (line-bx line))))
(defvar maxx (loop for line in input maximize (max (line-ax line) (line-bx line))))
(defvar miny (loop for line in input minimize (min (line-ay line) (line-by line))))
(defvar maxy (loop for line in input maximize (max (line-ay line) (line-by line))))

(defvar hv-map (make-array (list (1+ maxx) (1+ maxy))))

(loop for line in input
      if (or (equal (line-ax line) (line-bx line)) ; vertical
             (equal (line-ay line) (line-by line))) ; horizontal
      do (mark-line line hv-map))

(format t "hv overlaps: ~a~%"
        (loop for y from miny to maxy
              sum (loop for x from minx to maxx
                        sum (if (> (aref hv-map x y) 1) 1 0))))

(defvar all-map (make-array (list (1+ maxx) (1+ maxy))))

(loop for line in input
      do (mark-line line all-map))

(format t "all overlaps: ~a~%"
        (loop for y from miny to maxy
              sum (loop for x from minx to maxx
                        sum (if (> (aref all-map x y) 1) 1 0))))
