#lang racket

(require "./util.rkt" pict)

(define research-city
  (task "Research City"
        10 '()))

(define get-teacher
  (task "Get Teacher"
        10 (list research-city)))

(define get-tech-coord
  (task "Get Tech Coord"
        10 (list research-city)))

(define get-location
  (task "Get Location"
        10 (list research-city)))

(define get-5-students
  (task "Get 5 Students"
        10 (list research-city)))

(define run-first-class
  (task "Run First Class"
        60 
        (list
          get-tech-coord 
          get-teacher
          get-location
          get-5-students)))


(gantt 
  research-city
  (dep-graph run-first-class))
