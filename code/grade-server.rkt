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
; own grades.

; In order to run this module, you need to copy racets.rkt, racets-mlang.rkt, and
; facets.rkt from https://github.com/fordsec/racets
#lang reader "racets.rkt"

(define make-policy
  (lambda (player-name)
    (let-label l (lambda (x) (equal? x player-name)) l)))

(struct student (name policy grade-list))

(define student-with-policy
  (lambda (name grade-list) (student name (make-policy name) grade-list)))

; Some hard-coded data
(define alice (student-with-policy "Alice" '(93 84 87)))
(define bob (student-with-policy "Bob" '(84 71 73)))


(display "Nothing implemented yet!\n")
