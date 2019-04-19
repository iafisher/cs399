; This module defines a proof-of-concept for extending the Racets language
; (https://github.com/fordsec/racets) to handle bare identifiers correctly. Bare identifiers
; need to be wrapped in a function (defined as `var-wrapper` in this module) that checks if
; they are a boxed facet, and unboxes them if they are, in order to handle code like
;
;   (if k
;     (set! x 42)
;     (set! x 0))
;
; where if `k` is a facet, then `x` will end up as a box containing a facet instead of an
; integer as the rest of the code would expect. `x` will subsequently need to be unboxed
; wherever it is used. The variable wrapper defined in this module accomplishes that.
;
; The implementation uses the "languages-as-libraries" approach, in which a macro for
; #%module-begin is exported which walks the syntax tree and effects a whole-module
; transformation of the source code.
;
; You can configure other modules to use this language by putting #lang s-exp "racets.rkt" at
; the top of the module instead of #lang racket.
#lang racket


(struct facet (labelname left right) #:transparent)


(define (var-wrapper v)
  (if (and (box? v) (facet? (unbox v)))
    (unbox v)
    v))


(begin-for-syntax
  (define (transform-syntax stx)
    ; Recursively transform the syntax tree so that every bare identifier is wrapped by the
    ; `var-wrapper` function defined above.

    (syntax-case stx ()
      ; Match each form in fully-expanded Racket. Note that only forms which specifically
      ; require that some of their sub-parts are not recursively transformed need to be special-
      ; cased here, since a default case at the end which recursively walks the tree should
      ; handle most of the forms.
      ;
      ; See the Racket manual for details on fully-expanded Racket:
      ;   https://docs.racket-lang.org/reference/syntax-model.html#(part._fully-expanded)

      ; set!
      ([head id expr]
       ; Guard clause to ensure that the head of the list is set!
       (check-ident #'head #'set!)
       #`(head id #,(transform-syntax #'expr)))

      ; provide
      ([head a ...]
       (check-ident #'head #'#%provide)
       stx)

      ; #%plain-lambda
      ([head formals expr ...]
       (check-ident #'head #'#%plain-lambda)
       (datum->syntax stx
                      (cons #'head
                            (cons #'formals
                                  (map transform-syntax (syntax-e #'(expr ...)))))))

      ; quote
      ([head datum]
       (check-ident #'head #'quote)
       #'(head datum))

      ; TODO [2019-04-18]: let-values and letrec-values are not handled correctly.

      ; For any other list of forms, recursively transform each sub-form (except the first).
      ([a b ...]
       (datum->syntax stx (cons #'a (map transform-syntax (syntax-e #'(b ...))))))

      ; For any other individual form, wrap it if it is a symbol.
      (default
        (if (identifier? #'default)
          #`(var-wrapper #,#'default)
          #'default))))

  (define (check-ident ident expected)
    ; Check that the identifier is bound to the same reference as `expected`.
    (and (identifier? ident) (free-identifier=? ident expected))))


(define-syntax (module-begin stx)
  ; Intercept the entire module and transform it using `transform-syntax` on the fully-expanded
  ; syntax tree (which ensures that user macros are expanded first).
  (syntax-case stx ()
    [(_ forms ...)
     (let ([expanded (local-expand #'(#%plain-module-begin forms ...) 'module-begin '())])
       (transform-syntax expanded))]))


; Export everything from Racket, except replace #%module-begin with our implementation.
(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin])
         var-wrapper)
