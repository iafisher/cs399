; Testing code snippets I put in my draft to make sure they run as advertised.
#lang racket

(define-syntax (reverse-syntax stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-syntax 20 22 +)

(define-syntax (one-or-two stx)
  (syntax-case stx ()
    [(_ a b)
     #'(displayln "Got two")]
    [(_ a)
     #'(displayln "Got one")]))

(one-or-two 'a 'b)


(define-syntax (hygienic stx)
  (syntax-case stx ()
    [(_ a)
     #'(let ([x 10]) (+ x a))]))

(define x 32)
(hygienic x)
