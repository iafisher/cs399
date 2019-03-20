; Heavily based on https://docs.racket-lang.org/guide/hash-reader.html
#lang racket

(provide (rename-out [id-read read]
		     [id-read-syntax read-syntax]))


(define (id-read in)
  (parameterize ([current-readtable (make-id-readtable)])
    (read-syntax in)))

(define (id-read-syntax src in)
  (parameterize ([current-readtable (make-id-readtable)])
    (read-syntax src in)))

(define (make-id-readtable)
  (make-readtable (current-readtable)
		  #f 'non-terminating-macro read-id-wrapper))

(define read-id-wrapper
  (case-lambda
    [(ch in)
     (read-id ch in)]
    [(ch in src line col pos)
     (read-id ch in)]))

(define (read-id ch in)
  (let* ([s (string-append (string ch) (read-id-rec in))]
         [n (string->number s 10 'number-or-false)])
    (if (false? n) (string->symbol s) n)))

(define (read-id-rec in)
  (if (not (is-symbolic-char (peek-char in)))
      ""
      (let ([ch (read-char in)])
        (string-append (string ch) (read-id-rec in)))))

(define (is-symbolic-char ch)
  (or (char-alphabetic? ch) (char-numeric? ch)))