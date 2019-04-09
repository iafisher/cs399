#lang racket

(begin-for-syntax
  (define (tree-walk stx)
    (syntax-case stx ()
      ; Change this to [('#%plain-lambda formals body) and it blows up...
      [(#%plain-lambda formals body)
       (displayln "Got a plain lambda!")]
      [(head . tail)
       (tree-walk #'tail)])))


(define-syntax (test-expand stx)
  (syntax-case stx ()
    [(_ forms ...)
     (begin
       (define expanded (local-expand #'(values forms ...) 'expression '()))
       (tree-walk expanded)
       #'(void))])
  #'(void))


; To avoid "undefined variable" error
(define (not-lambda x y) (void))

(test-expand (not-lambda '(x) 'x))
(test-expand (lambda (x) x))

#|
(define-syntax (test-macro stx)
  (syntax-case stx ()
    [(_ ('lambda formals body ...))
     #'(displayln "I got a lambda!")]
    [(_ default)
      #'(displayln "I didn't get a lambda!")]))

;(test-macro (lambda (x) x x x))
;(test-macro x)
|#
