; Put #lang s-exp "fully-expand-me.rkt" at the top of your module to see its full expansion.
#lang racket

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (let ([as-datum
             (syntax->datum
               (local-expand #'(#%plain-module-begin forms ...) 'module-begin '()))])
       (displayln as-datum)
       #'(#%plain-module-begin))]))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin]))
