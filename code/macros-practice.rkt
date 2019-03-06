; Playground for Racket's syntactic macros.
; From "Fear of Macros" (https://www.greghendershott.com/fear-of-macros/all.html)
;
; Important functions:
;
;   define-syntax: Whenever the symbol is encountered, apply the syntax
;   transformation.
;
;   syntax-case: Pattern matching for syntax objects.
;
;   define-syntax-rule: Shortcut for defining a macro with pattern matching.
;
;   syntax->datum: Recursively convert a syntax object to an S-expression.
;
;   syntax-e: Convert the top level of a syntax object to an S-expression.
;
;   begin-for-syntax: Opens a block for defining helper functions that can
;   be used at compile time.
#lang racket

; Only racket/base is defined automatically at compile-time, so we have to
; specifically request pattern-matching if we want to use it in a macro.
(require (for-syntax racket/match))


; Ignore the input syntax and always expand to the same string literal.
(define-syntax (expand-to-string stx) 
  ; #'x is short for (syntax x).
  #'"I am an expanded macro!")

(expand-to-string)
expand-to-string  ; Doesn't have to be in parentheses.
(expand-to-string whatever goes here doesn't matter)


; Reverse the arguments.
(define-syntax (reverse-syntax stx)
  (datum->syntax 
    ; The first argument contains the lexical content information that is
    ; associated with the output syntax. Can be #f to associate no lexical
    ; content.
    stx 
    (reverse 
      ; cdr because the first element of the S-expression is the name of the
      ; macro, "reverse-syntax".
      (cdr (syntax->datum stx)))))

(reverse-syntax 20 22 +)


; Redefinition of the lazy if expression.
(define-syntax (my-if stx)
  (match (syntax->list stx)
    [(list name condition true-expr false-expr)
     ; I don't fully understand how this avoids evaluating the false case.
     (datum->syntax stx `(cond [,condition ,true-expr]
			       [else ,false-expr]))]))

(my-if #t (displayln "true") (displayln "false"))


; my-if using syntax-case instead of match
(define-syntax (my-if-2 stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr]
	     [else false-expr])]))

(my-if-2 #t (displayln "true2") (displayln "false2"))


; my-if using define-syntax-rule
(define-syntax-rule (my-if-3 condition true-expr false-expr)
  (cond [condition true-expr]
	[else false-expr]))

(my-if-3 #t (displayln "true3") (displayln "false3"))


(define-syntax (hyphen-define stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a
				 (string->symbol (format "~a-~a"
							 (syntax->datum #'a)
							 (syntax->datum #'b))))
       ()
       [name #'(define (name args ...)
		 body0 body ...)])]))

(hyphen-define foo bar () #t)
(foo-bar)
