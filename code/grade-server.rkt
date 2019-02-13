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
    (let-label l (lambda (x) (equal? x player-name)) l)))

;(struct student (name policy grade-list))

;(define student-with-policy
;    (lambda (name grade-list)
;      (let ([policy (make-policy name)])
;	(student name policy (fac policy grade-list '())))))

; Some hard-coded data
;(define alice (student-with-policy "Alice" '(93 84 87)))
;(define bob (student-with-policy "Bob" '(84 71 73)))
;(define students (list alice bob))
(define alice-policy (make-policy "Alice"))
(define bob-policy (make-policy "Bob"))

(define alice-grades (fac alice-policy '(93 84 87) '()))
(define bob-grades (fac bob-policy '(84 71 73) '()))


;(define print-grades
;  (lambda (arg student)
;    (displayln (obs (student-policy student) arg (student-grade-list student)))))

(define print-alice-grades
  (lambda (arg)
    (displayln (obs alice-policy arg alice-grades))))

(define print-bob-grades
  (lambda (arg)
    (displayln (obs bob-policy arg bob-grades))))

; How do I implement this?
(define print-average-grade (lambda (student-list) 0))

(display "Alice's grades, viewed by Alice: ")
(print-alice-grades "Alice")

(display "Alice's grades, viewed by Bob: ")
(print-alice-grades "Bob")
