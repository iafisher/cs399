#lang s-exp "racets.rkt"

(define (my-map f xs)
  (if (null? xs)
    '()
    (cons (f (car xs)) (my-map f (cdr xs)))))

(my-map displayln '(1 2 3))
