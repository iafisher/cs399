#lang racket

(require (for-syntax racket/match))

(define (var-wrapper v vstr)
  (display "Dereferencing ")
  (displayln vstr)
  v)

(begin-for-syntax
  (define (wrap-variable v)
    (let ([vstr (symbol->string v)])
      (list 'var-wrapper v vstr)))
      ;`(begin (display "Dereferencing ") (displayln ,vstr) ,v)))

  (define (transform-syntax datum)
    (match datum
      [(list '#%app f xs ...)
       (cons f (map (lambda (x) (if (symbol? x) (wrap-variable x) (transform-syntax x))) xs))]
      [(list xs ...) (map transform-syntax xs)]
      [default default])))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (let ([as-datum 
	     (syntax->datum 
	       (local-expand #'(#%plain-module-begin forms ...) 'module-begin '()))])
       (datum->syntax stx (transform-syntax as-datum)))]))

; Export everything from Racket, except replace #%module-begin with our implementation.
(provide (except-out (all-from-out racket) #%module-begin)
	 (rename-out [module-begin #%module-begin]))
