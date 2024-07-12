(in-package :cl-user)
(defpackage :aoc2023
  (:use :cl :arrow-macros)
  (:local-nicknames (:g :cl-graph))
  (:export :day-01
           :day-02
           :day-03
           :day-04
           :day-05
           :day-06
           :day-07
           :day-08
           :day-09
           :day-10
           :day-11
           :day-12
           :day-13
           :day-25))

;; didn't seem to do much for these early ones.
;;(declaim (optimize (speed 3) (safety 0)))


;;;; TODOs
;;;; - refactoring
;;;; - figure out dictionary library
;;;; - can i make a default dict somehow?
;;;; 
