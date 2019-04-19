#lang reader "racets.rkt"

(define alice-policy (let-label l (lambda (x) (equal? x "Alice")) l))
(define bob-policy (let-label l (lambda (x) (equal? x "Bob")) l))

(define my-facet (fac alice-policy 42 0))

(display "Facet as observed by Alice: ")
(displayln (obs alice-policy "Alice" my-facet))

(display "Facet as observed by Bob: ")
(displayln (obs alice-policy "Bob" my-facet))

; Not sure what the expected behavior of these is.
(display "Facet as observed by Alice with's Bob policy: ")
(displayln (obs bob-policy "Alice" my-facet))

(display "Facet as observed by Bob with's Bob policy: ")
(displayln (obs bob-policy "Bob" my-facet))
