#lang reader "racets.rkt"

(define alice-policy (let-label l (lambda (x) (equal? x "Alice")) l))
(define k (fac alice-policy #t #f))

(define x 10)

(if k
  (set! x 42)
  (set! x 0))

(displayln x)
