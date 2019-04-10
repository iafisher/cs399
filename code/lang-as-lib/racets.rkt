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

(define (var-wrapper v vstr)
  (display "Reading variable `")
  (display vstr)
  (displayln "`")
  v)

(begin-for-syntax
  (define (transform-syntax stx)
    ;(displayln (syntax->datum stx))
    (syntax-case stx ()
      ; Match each form in fully-expanded Racket.
      ; https://docs.racket-lang.org/reference/syntax-model.html#(part._fully-expanded)

      ; set!
      ([head id expr]
       (free-identifier=? #'head #'set!)
       #`(head id #,(transform-syntax #'expr)))

      ; provide
      ([head a ...]
       (and (identifier? #'head) (free-identifier=? #'head #'#%provide))
       stx)

      ; #%plain-lambda
      ([head formals expr ...]
       (and (identifier? #'head) (free-identifier=? #'head #'#%plain-lambda))
       (datum->syntax stx
                      (cons #'head
                            (cons #'formals
                                  (map transform-syntax (syntax-e #'(expr ...)))))))

      ; Any other list.
      ([a b ...]
       (datum->syntax stx (cons #'a (map transform-syntax (syntax-e #'(b ...))))))

      ; Any other individual form.
      (default
        (if (identifier? #'default)
          (wrap-variable #'default)
          #'default))))

  (define (wrap-variable v)
    (let ([vstr (symbol->string (syntax->datum v))])
      #`(var-wrapper #,v #,vstr))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (let ([expanded (local-expand #'(#%plain-module-begin forms ...) 'module-begin '())])
       (transform-syntax expanded))]))

; Export everything from Racket, except replace #%module-begin with our implementation.
(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin])
         var-wrapper)
