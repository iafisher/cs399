; A prototype grade server that demonstrates the use of faceted execution.
;
; An individual user of the grade server may have one of three roles: professor, TA, or
; student.
;
; Each student has a list of assignments with a numeric grade and optionally comments
; from the grader.
;
; Professors may see the grades of every student for all assignments. TAs may see the
; grades of every student, but only for the last assignment. Students may only see their
; own individual grades, but they may see the average grade of everyone in the class.

; In order to run this module, you need to copy racets.rkt, racets-mlang.rkt, and
; facets.rkt from https://github.com/fordsec/racets
#lang reader "racets.rkt"

(define make-policy
  (lambda (player-name)
    (let-label l (lambda (x) (or (equal? x player-name) (equal? x "professor"))) l)))

;(struct student (name policy grade-list))

;(define student-with-policy
;    (lambda (name grade-list)
;      (let ([policy (make-policy name)])
;	(student name policy (fac policy grade-list '())))))

; Some hard-coded data
;(define alice (student-with-policy "Alice" '(93 84 87)))
;(define bob (student-with-policy "Bob" '(84 71 73)))
;(define students (list alice bob))
;(define alice-policy (make-policy "Alice"))
;(define bob-policy (make-policy "Bob"))

;(define alice-grades (fac alice-policy '(93 84 87) '()))
;(define bob-grades (fac bob-policy '(84 71 73) '()))


(define student-names '())
(define student-grades '())
(define student-policies '())


(define add-student
  (lambda (name grades)
    (let ([policy (make-policy name)])
      (begin
	(set! student-names (cons name student-names))
	(set! student-grades (cons (fac policy grades '()) student-grades))
	(set! student-policies (cons policy student-policies))))))


; Fetch the unprotected grades of everyone in the class.
; This function is not privacy safe!
(define reveal-grades
  (letrec ([reveal-grades-rec
	  (lambda (grade-list policy-list arg)
	    (if (empty? policy-list)
	      grade-list
	      (obs
		(car policy-list)
		arg
		(reveal-grades-rec grade-list (cdr policy-list) arg))))])
    (lambda (arg) (reveal-grades-rec student-grades student-policies arg))))


; Fetch the faceted grade list for the given student.
(define fetch-grade
  (lambda (name)
    (let ([index (index-of student-names name)])
      (if index
	(fac
	  (list-ref student-policies index)
	  (list-ref (reveal-grades "professor") index)
	  '())
	'()))))


; Helper function courtesy of https://stackoverflow.com/questions/15871042/
(define (index-of lst elem)
  (let loop ((lst lst)
	     (idx 0))
    (cond ((empty? lst) #f)
	  ((equal? (car lst) elem) idx)
	  (else (loop (cdr lst) (add1 idx))))))


; How do I implement this?
(define print-average-grade
  (lambda (student-id)
    0))
;(let ([alice-grade (compute-grade (obs alice-policy "professor" alice-grades))]
;  [bob-grade (compute-grade (obs bob-policy "professor" bob-grades))])
;     (displayln (/ (+ alice-grade bob-grade) 2)))))

(define compute-grade
  (lambda (grade-list)
    (if (= (length grade-list) 0)
      0
      (/ (apply + grade-list) (length grade-list)))))


; The mock API endpoints. The first argument identifies the entity requesting the
; action.
(define print-grade
  (lambda (observer name)
    (let ([policy (list-ref student-policies (index-of student-names name))])
      (displayln (obs policy observer (fetch-grade name))))))


; Populate the student database. Note that add-student prepends students to the
; list, so that Alice will have ID 1 and Bob will have ID 0.
(add-student "Alice" '(93 84 87))
(add-student "Bob" '(84 71 73))


; Some sample API calls.
(print-grade "Alice" "Alice")
(print-grade "professor" "Alice")
(print-grade "Bob" "Alice")
