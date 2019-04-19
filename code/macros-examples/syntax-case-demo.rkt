#lang racket

(define-syntax (variadic stx)
  (syntax-case stx ()
    [(_ a)
     #'(displayln "got one argument")]
    [(_ a b)
     #'(displayln "got two arguments")]))

(variadic 1)
(variadic 1 2)
