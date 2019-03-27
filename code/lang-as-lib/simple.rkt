#lang racket


; Fully expands the module and prints out its abstract syntax tree, before running it normally.
(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (with-syntax ([(_ core-forms ...)
                    (local-expand #'(#%plain-module-begin forms ...) 'module-begin '())])
       #'(#%plain-module-begin (displayln '(core-forms ...)) core-forms ...))]))


; https://docs.racket-lang.org/guide/module-languages.html#%28part._s-exp%29
(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin]))
