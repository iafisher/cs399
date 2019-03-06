#lang racket


; Print the source location of the macro's usage.
(define-syntax (print-source-location stx)
  (datum->syntax
    stx
    `(displayln (format "line ~a of ~a" ,(syntax-line stx) ,(syntax-source stx)))))


; Define a syntax for iterating over lists.
(define-syntax-rule (my-for (id lst) body)
  (letrec ([f (lambda (rec-lst)
	     (if (empty? rec-lst)
	       (void)
	       (begin
		 (let ([id (car rec-lst)])
		   body)
		 (f (cdr rec-lst)))))])
    (f lst)))


; Export everything in the module.
(provide (all-defined-out))
