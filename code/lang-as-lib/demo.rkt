; A module that demonstrates many different Racket forms to test the Racets implementation on.
#lang s-exp "racets.rkt"

(displayln "Inside demo.rkt")

(define two 10)
(define one 32)

; Various constructs using variables
(define three one)
(+ two three)
((lambda (four) four) 14)

(letrec ([five 5] [six five]) six)

(define seven 7)
(set! one seven)

(lambda (x y) 10)

(quote fifteen)

(provide three)
