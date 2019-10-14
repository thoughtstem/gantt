#lang racket

(require pict graph "./util.rkt")

(define (task->pict t)
  (cc-superimpose
    (rectangle (task-days t) 20)
    (text (task-name t))))

;Assume we're getting a dep-graph
(define (gantt start g)
  (define preds (get-neighbors g start))  
  (define rect (task->pict start))
  
  rect)



