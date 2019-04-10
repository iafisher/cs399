#lang racket

(begin-for-syntax
  (define (tree-walk stx)
    (syntax-case stx ()
      [(lambda-form formals body)
       ;(if (free-identifier=? #'lambda-form #'#%plain-lambda)
       (displayln (syntax->datum #'lambda-form))
       (if (free-identifier=? #'lambda-form #'lambda)
         (displayln "Got a plain lambda!")
         (begin
           (displayln "Did not get a plain lambda!")
           ;(tree-walk #'formals)
           (tree-walk #'body)
           ;))]
           ))]
      [(forms ...)
       (displayln "Hit the second case")
       (map tree-walk (syntax-e #'(forms ...)))])))


(define-syntax (test-expand stx)
  (syntax-case stx ()
    [(_ forms ...)
     (begin
       (define expanded (local-expand #'(values forms ...) 'expression '()))
       (displayln (syntax->datum expanded))
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
