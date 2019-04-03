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
  (define (transform-syntax datum)
    ; Match each form in fully-expanded Racket.
    ; https://docs.racket-lang.org/reference/syntax-model.html#(part._fully-expanded)
    (match datum
      ; #%app
      [(list '#%app f xs ...)
       (cons (transform-syntax f) (map transform-syntax xs))]

      ; define-values
      [(list 'define-values ids vs ...)
       (cons 'define-values (cons ids (map transform-syntax vs)))]

      ; let-values
      [(list 'let-values (list formals ...) xs ...)
       (cons 'let-values
             ; Each member of formals has the form [(id ...) expr]
             (cons (map (lambda (formal) (list (car formal) (transform-syntax (cadr formal)))))
                   (map transform-syntax xs)))]

      ; set!
      [(list 'set! id expr)
       (list 'set! id (transform-syntax expr))]

      ; #%plain-module-begin
      [(list '#%plain-module-begin xs ...)
       (cons '#%plain-module-begin (map transform-syntax xs))]

      ; quote
      [(list 'quote datum)
       `(quote ,(transform-syntax datum))]

      ; lambda (not sure why this isn't expanded into #%plain-lambda...)
      [(list 'lambda formals exprs ...)
       (cons 'lambda (cons formals (map transform-syntax exprs)))]

      ; #%expression
      [(list '#%expression e)
       (list '#%expression (transform-syntax e))]

      ; Any other list of forms. This case should come second-to-last.
      [(list head xs ...)
       (cons head (map transform-syntax xs))]

      ; Any other individual form. This case should come last.
      [default
       (if (symbol? default)
         (wrap-variable default)
         default)]))

  (define (wrap-variable v)
    (let ([vstr (symbol->string v)])
      (list 'var-wrapper v vstr))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (let ([as-datum
             (syntax->datum
               (local-expand #'(#%plain-module-begin forms ...) 'module-begin '()))])
       (datum->syntax stx (transform-syntax as-datum)))]))

; Export everything from Racket, except replace #%module-begin with our implementation.
(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin])
         var-wrapper)
