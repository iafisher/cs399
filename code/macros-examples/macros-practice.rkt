#lang racket
(require "macros-practice-def.rkt")


(my-for (x '(1 2 3)) (displayln x))

(print-source-location)

; Test that hygiene is preserved.
(define (f x) (displayln x))
(my-for (x '(1 2 3)) (f x))
