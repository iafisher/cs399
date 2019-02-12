#lang reader "racets.rkt"

(define policy (let-label l (lambda (x) (equal? x "Alice")) l))

(define my-facet (fac policy 42 0))

(display "Facet as observed by Alice: ")
(displayln (obs policy "Alice" my-facet))

(display "Facet as observed by Bob: ")
(displayln (obs policy "Bob" my-facet))
