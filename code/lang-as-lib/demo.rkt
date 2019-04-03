;#lang s-exp "racets.rkt"
#lang s-exp "fully-expand-me.rkt"

(displayln "Inside demo.rkt")

(define two 10)
(define one 32)

; Various constructs using variables
(define three one)
(+ two three)
((lambda (four) four) 14)
