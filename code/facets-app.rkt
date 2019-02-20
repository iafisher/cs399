#lang reader "racets.rkt"

(define square (lambda (x) (* x x)))

(define alice-policy (let-label l (lambda (x) (equal? x "Alice")) l))

(define faceted-int (fac alice-policy 8 0))
; faceted-square-int is now a facet of the form (fac alice-policy 64 0)
(define faceted-square-int (square faceted-int))

; Prints 64 (high-confidentiality value).
(display "Observed by Alice: ")
(displayln (obs alice-policy "Alice" faceted-square-int))
; Prints 0 (low-confidentiality value).
(display "Observed by Bob: ")
(displayln (obs alice-policy "Bob" faceted-square-int))
