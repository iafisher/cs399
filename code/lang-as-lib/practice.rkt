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
    [(_ forms ...)
     (begin
       (define expanded (local-expand #'(#%plain-module-begin forms ...) 'module-begin '()))
       (displayln expanded))])

  (let ([fixed (datum->syntax stx (cons '#%plain-module-begin (cdr (syntax->datum stx))))])
    (define expanded (local-expand fixed 'module-begin '()))
    (tree-walk expanded)
    (displayln expanded))
  #'(void))


(test-macro (lambda (x) x x x))
(test-macro x)
(test-expand (lambda (x) x))
