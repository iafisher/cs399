#lang racket


(define-syntax (attempt stx)
  (define expanded (local-expand (datum->syntax stx (cadr (syntax->datum stx))) 'expression '()))
  ;(displayln expanded)
  (syntax-case expanded ()
    #|
    ([(~literal #%plain-lambda #:phase -1) formals body]
     (displayln "Got a plain lambda!"))
    ([(~literal lambda #:phase -1) formals body]
     (displayln "Got a regular lambda!"))
    |#
    ([f formals body]
     (if (free-identifier=? #'f #'#%plain-lambda)
       (displayln "Got a plain lambda using free-identifier!")
       (displayln "nope!")))
    ([a ...]
     (displayln "Got something else!"))
    )

  #'(void)
  )

(attempt (lambda (x) x))
