; This module defines a Racket-like language that is identical to normal Racket except that
; whenever an identifier is used, the identifier is printed, e.g.
;
;   (define x 10)
;   (+ x 32)
;
; will print "Dereferencing x".
;
; This language is a proof-of-concept for implementing Racets, which will similarly need to
; transform bare identifiers in the source code.
;
; The implementation uses the "languages-as-libraries" approach, in which a macro for
; #%module-begin is exported which walks the syntax tree and effects a whole-module
; transformation of the source code.
;
; You can configure other modules to use this language by putting #lang s-exp "racets.rkt" at
; the top of the module instead of #lang racket.
#lang racket

(require (for-syntax racket/match))

(define (var-wrapper v vstr)
  (display "Reading variable `")
  (display vstr)
  (displayln "`")
  v)

(begin-for-syntax
  ; TODO [2019-04-04]: Transform this to using syntax-case.
  (define (transform-syntax stx)
    (syntax-case stx ()
    ; Match each form in fully-expanded Racket.
    ; https://docs.racket-lang.org/reference/syntax-model.html#(part._fully-expanded)
      [(#%app f xs ...)
       #`(#,(transform-syntax f) #,(map transform-syntax (syntax-e #'(xs ...))))]

      [(define-values ids vs ...)
       #`(define-values ids #,(map transform-syntax (syntax-e #'(vs ...))))]

      #|
      [(let-values (formals ...) xs ...)
       ; Each member of formals has the form [(id ...) expr]
       (let ([new-formals (map (lambda (formal) (list (car formal) (transform-syntax (cadr formal)))) (syntax->list formals))])
         #`(let-values new-formals ,(map transform-syntax xs)))]
      |#

      [(set! id expr)
       #`(set! id #,(transform-syntax expr))]

      [(#%plain-module-begin xs ...)
       #`(#%plain-module-begin #,(map transform-syntax (syntax-e #'(xs ...))))]

      [(quote datum)
       #`(quote #,(transform-syntax datum))]

      ; Not sure why this isn't expanded into #%plain-lambda...
      [(lambda formals exprs ...)
       #`(lambda formals #,(map transform-syntax (syntax-e #'(exprs ...))))]

      [(#%expression e)
       #`(#%expression #,(transform-syntax e))]

      ; Any other list of forms. This case should come second-to-last.
      [(head xs ...)
       #`(head #,(map transform-syntax #'(xs ...)))]

      ; Any other individual form. This case should come last.
      [default
       (if (symbol? (syntax->datum #'default))
         #'(wrap-variable default)
         #'default)]))

  (define (wrap-variable v)
    (let ([vstr (symbol->string v)])
      (list 'var-wrapper v vstr))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (let ([expanded (local-expand #'(#%plain-module-begin forms ...) 'module-begin '())])
       (transform-syntax expanded))]))

; Export everything from Racket, except replace #%module-begin with our implementation.
(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin])
         var-wrapper)
