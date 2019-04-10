#lang s-exp "racets2.rkt"
;#lang s-exp "fully-expand-me.rkt"

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
