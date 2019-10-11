#lang racket

(provide (struct-out task)
         dep-chart
         dep-graph
         gantt)

(require pict graph rsvg)

(struct task (name days deps) #:transparent)


(define (render gv)
  (with-output-to-file "out.dot"
                       #:exists 'replace
                       (thunk (displayln gv)))
  (system (~a "dot -Grankdir=LR -Tsvg out.dot -o out.svg"))

  (svg-file->pict "out.svg"))


(define g (unweighted-graph/directed '()))
(define (dep-graph t)
  (add-vertex! g t)
  (for ([d (task-deps t)])
    (add-vertex! g d)
    (add-directed-edge! g d t)
    (dep-graph d)) 
  g)

(define (dep-chart t)
  (define g (dep-graph t))
  (render (graphviz g)))


(define (task->pict t)
  (cc-superimpose
    (rectangle (task-days t) 20)
    (text (task-name t))))


(define (gantt start g)
  (define preds (get-neighbors g start))  

  (define rect (task->pict start))


  
  rect)



