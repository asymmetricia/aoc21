#!/usr/bin/env -S sbcl --script

(require "asdf")
(require "split-sequence")

(defvar lines (with-open-file (f "a/input")
                (loop for line = (read-line f nil)
                      while line
                      collect line)))
