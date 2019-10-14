#lang racket

(provide (except-out 
           (struct-out task)
           task)
         (rename-out
           [make-task task])
         dep-chart
         dep-graph
         render-graph)

(require pict graph rsvg
         posn
         "./map-graph.rkt")

(struct task (name days deps start)
  #:transparent)

(define (make-task name days deps)
  (task name days deps
        (if (empty? deps)
          0
          (apply max
            (map task-end deps)))))

(define (task-end t)
  (+ (task-start t)
     (task-days t)))

(define (task->string t)
  (~a
    (task-name t)
    ": "
    (task-start t)))

(define (task-id t)
  (string-replace (task-name t)
                   " " ""))

(define (task->rect t)
  (define day-scale-factor 10)

  (define day-square
    (filled-rounded-rectangle
      10 10
      #:color "red"))

  (apply hc-append
         (map (const day-square) 
         (range (task-days t)))))

(define (start? t)
  (string=? (task-name t) "START"))

(define (end? t)
  (string=? (task-name t) "END"))

(define (task->pict t)
  (cond
    [(or (start? t)
         (end? t)) 
     (rotate (filled-rounded-rectangle 10 10 #:color "black")
             (/ pi 4))]
    [else (task->rect t)]))

(define (task->position sorted-ts t)
  (define y (index-of sorted-ts t))
  (define x 
    (+ (/ (task-days t) 2)
       (task-start t)))

  (posn x y))

(define (padding n)
  (colorize (hline (* 10 n) 0)
            "lightgray"))

(define (viz-vertex t)
  (list
    (hc-append 
      (lc-superimpose
        (colorize (rectangle 200 20) "lightgray")
        (text (task-name t)))
      (padding 
        (if (start? t)
          0
          (add1 (task-start t)))))
    (task->pict t)))

(define (sort-by-start-then-days ts)
  (sort ts
        (lambda (t1 t2)
          (define st1 (task-start t1))
          (define st2 (task-start t2))
          (if (= st1 st2)
            (< (task-days t1) (task-days t2))
            (< st1 st2)))))

(define (viz-arrow r1 r2 main)
  (pin-arrow-line 5 main
                  r1 rc-find
                  r2 lc-find
                  #:start-angle 
                  (- (/ pi 4))
                  #:end-angle   (* 2 pi) ))

(define (viz-arrows es vs rects main)
  (define curr main)
  (for ([e es])
    (define i1 (index-of vs (first e)))
    (define i2 (index-of vs (second e)))

    (when (and i1 i2)
      (set! curr
        (viz-arrow (list-ref rects i1) 
                   (list-ref rects i2)
                   curr))))

  curr)

(define (dep-graph->pict g)
  (define sorted-vs
    (sort-by-start-then-days 
      (filter-not end? 
                  (get-vertices g))))

  (define row-parts (map viz-vertex sorted-vs))

  (define rows (map (curry apply hc-append) row-parts))

  (define main
    (apply vl-append rows))
  
  (viz-arrows (get-edges g)
              sorted-vs
              (map second row-parts)
              main))


(define (render-graph g)
  (dep-graph->pict g))


(define (dep-graph-main 
          #:graph 
          (g (unweighted-graph/directed '()))
          . ts)

  (for ([t ts])
    (add-vertex! g t)
    (for ([d (task-deps t)])
      (add-vertex! g d)
      (add-directed-edge! g d t)
      (dep-graph-main #:graph g d))) 

  g)


(define (dep-graph . ts)
  (add-start/end!
    (apply dep-graph-main ts)) )

(define (add-start/end! g)
  ;End
  (define sinks (get-sinks g)) 
  (define end   (make-task "END" 0 sinks))
  (add-vertex! g end)
  (for ([s sinks])
    (add-directed-edge! g s end))

  ;Start
  (define sources (get-sources g)) 
  (define start (make-task "START" 0 '()))
  (add-vertex! g start)
  (for ([s sources])
    (add-directed-edge! g start s))
  
  g)

(define (dep-chart g)
  (render-graph g))


