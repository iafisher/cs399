; A prototype grade server that demonstrates the use of faceted execution.
;
; An individual user of the grade server may have one of three roles: professor, student or
; stranger (i.e., someone who is not logged in).
;
; Each student has a list of assignments with a numeric grade. Professors may see the 
; grades of every student for all assignments. Students may only see their own individual 
; grades, but they may see the average grade of everyone in the class. Strangers may not
; access any information.

; In order to run this module, you need to copy racets.rkt, racets-mlang.rkt, and
; facets.rkt from https://github.com/fordsec/racets
#lang reader "racets.rkt"


;;; Global "database" ;;;
(define student-names '())
(define student-grades '())
(define student-policies '())


; Make a policy for a particular student.
(define (make-policy student-name)
  (let-label l (lambda (x) (or (equal? x student-name) (equal? x "professor"))) l))

; The open policy allows access to anyone who is logged in.
(define open-policy
  (let-label l (lambda (x) (not (equal? x "stranger"))) l))

; Fetch the unprotected grades of everyone in the class.
; This function is not privacy safe!
(define (reveal-grades arg)
  ; Define a recursive helper function.
  (letrec ([reveal-grades-rec
	  (lambda (grade-list policy-list arg)
	    (if (empty? policy-list)
	      grade-list
	      (obs
		(car policy-list)
		arg
		(reveal-grades-rec grade-list (cdr policy-list) arg))))])
    (reveal-grades-rec student-grades student-policies arg)))

; Fetch the faceted class average value.
(define (fetch-class-average)
  (let* ([grades (reveal-grades "professor")]
	[total (foldr + 0 (map compute-grade grades))])
    (fac open-policy (/ total (length grades)) 0)))

; Fetch the faceted grade list for the given student.
(define (fetch-grade name)
  (let ([index (index-of student-names name)])
    (if index
      (fac
	(list-ref student-policies index)
	(list-ref (reveal-grades "professor") index)
	'())
      '())))

; Given a student's list of grades, compute their class class.
(define (compute-grade grade-list)
  (if (= (length grade-list) 0)
    0
    (/ (apply + grade-list) (length grade-list))))


;;; The mock API endpoints. ;;;
; The first argument identifies the entity requesting the
; action, while the second argument identifies the target of the action.

(define (print-grade observer name)
  (let ([policy (list-ref student-policies (index-of student-names name))])
    (displayln (obs policy observer (fetch-grade name)))))

(define (print-class-average observer)
  (displayln (obs open-policy observer (fetch-class-average))))

(define (add-student name grades)
  (let ([policy (make-policy name)])
    (begin
      (set! student-names (cons name student-names))
      (set! student-grades (cons (fac policy grades '()) student-grades))
      (set! student-policies (cons policy student-policies)))))


; Populate the student database. Note that add-student prepends students to the
; list, so that Alice will have ID 1 and Bob will have ID 0.
(add-student "Alice" '(93 84 87))
(add-student "Bob" '(84 71 73))

; Some sample API calls.
(display "Alice's grades, observed by Alice: ")
(print-grade "Alice" "Alice")
(display "Alice's grades, observed by the professor: ")
(print-grade "professor" "Alice")
(display "Alice's grades, observed by Bob: ")
(print-grade "Bob" "Alice")
(display "\n")

(display "Class average, observed by Alice: ")
(print-class-average "Alice")
(display "Class average, observed by a stranger: ")
(print-class-average "stranger")
