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
(require (for-syntax racket/match racket/syntax))


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


; hyphen-define rewritten to use format-id and with-syntax
(define-syntax (hyphen-define-2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body0 body ...))]))

(hyphen-define-2 foo bar-2 () #t)
(foo-bar-2)


; Redefinition of (a simplified version of) the standard struct macro
(define-syntax (our-struct stx)
    (syntax-case stx ()
      [(_ id (fields ...))
       (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
         #`(begin
             ; Define a constructor.
             (define (id fields ...)
               (apply vector (cons 'id  (list fields ...))))

             ; Define a predicate.
             (define (pred-id v)
               (and (vector? v)
                    (eq? (vector-ref v 0) 'id)))

             ; Define an accessor for each field.
             #,@(for/list ([x (syntax->list #'(fields ...))]
                           [n (in-naturals 1)])
                  (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                                [ix n])
                    #`(define (acc-id v)
                        (unless (pred-id v)
                          (error 'acc-id "~a is not a ~a struct" v 'id))
                        (vector-ref v ix))))))]))

(our-struct test-struct (a b))
(define s (test-struct 42 666))
(test-struct-a s)
