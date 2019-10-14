#lang racket

(provide map-graph
         get-sources
         get-sinks)

(require graph)

(define (clear-graph g)
  (for ([v (get-vertices g)])
    (remove-vertex! g v))
  g)


;Note: If f does not map all vs to unique values, you'll get fewer vertices in the returned graph.  This can be confusing if you're used to mapping over lists.   
(define (map-graph f g)
  (define old-vs (get-vertices g))  
  (define new-vs (map f old-vs))

  (define v-hash 
    (apply hash 
           (flatten (map list old-vs new-vs))))


  ;Make sure it's the same type...
  ;  But then again we add directed edges later which assumes a type.
  ;  Not really that general yet.  But maybe a good start.
  (define new-g (clear-graph (graph-copy g)))

  (for ([v old-vs])
    (define new-v (hash-ref v-hash v))   
    (add-vertex! new-g new-v))

  (for ([v old-vs])
    (define new-v (hash-ref v-hash v))   
    (for ([old-n (get-neighbors g v)])
      (define new-n (hash-ref v-hash old-n))   

      (add-directed-edge! new-g new-v new-n)))
  
  new-g)

(define (has-no-neighbors-in g v)
  (zero? (length (get-neighbors g v))))

(define (get-sinks g)
  (filter (curry has-no-neighbors-in g) 
          (get-vertices g)))

(define (get-sources g)
  (define tg (transpose g))
  (filter (curry has-no-neighbors-in tg) 
          (get-vertices tg)))

(module+ test
  (require rackunit)

  (define g (unweighted-graph/directed '())) 

  (add-vertex! g "A")
  (add-vertex! g "B")
  (add-vertex! g "C")
  (add-directed-edge! g "A" "B")
  (add-directed-edge! g "B" "C")

  (define (double s)
    (~a s s))


  (check-true
    (has-vertex?
      (map-graph double g)   
      "AA"))

  (check-equal?
    (length (get-vertices (map-graph double g)))   
    3)

  (check-equal?
    (length (get-edges (map-graph double g)))   
    2)
  
  (check-equal?
    (get-sources g)
    '("A"))

  (check-equal?
    (get-sinks g)
    '("C")))


