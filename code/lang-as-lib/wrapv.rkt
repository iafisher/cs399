#lang racket

(define-syntax-rule (module-begin body ...)
  (#%plain-module-begin
   (displayln "Using the wrapv macro language!\n") 
   body ...))

; https://docs.racket-lang.org/guide/module-languages.html#%28part._s-exp%29
(provide (except-out (all-from-out racket) #%module-begin #%variable-reference)
	 (rename-out [module-begin #%module-begin]))
