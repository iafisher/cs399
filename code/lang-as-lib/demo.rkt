#lang s-exp "racets.rkt"

(displayln "Inside demo.rkt")

(define x 10)
(define y 32)

; Various constructs using variables
(define z y)
(displayln (+ x y))
