#lang racket

(define-syntax (test-macro stx)
  (syntax-case stx ()
    [(test-macro (lambda formals body ...))
     #'(displayln "I got a lambda!")]
    [(_ default)
      #'(displayln "I didn't get a lambda!")]))


(begin-for-syntax
  (define (tree-walk stx)
    (syntax-case stx ()
      [(#%plain-lambda formals body)
       (displayln "Got a plain lambda!")]
      [(head . tail)
       (tree-walk #'tail)])))


(define-syntax (test-expand stx)
  (syntax-case stx ()
    [(_ form)
     (begin
       (define expanded (local-expand #'form 'expression '()))
       (tree-walk expanded)
       (displayln expanded)
       #'(void))])
  #'(void))


;(test-macro (lambda (x) x x x))
;(test-macro x)
(test-expand (lambda (x) x))
