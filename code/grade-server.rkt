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


(define print-grades
  (lambda (student-id)
    (displayln 
      (obs (list-ref student-policies student-id)
	   (list-ref student-names student-id)
	   (list-ref student-grades student-id)))))

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


; Populate the student database. Note that add-student prepends students to the
; list, so that Alice will have ID 1 and Bob will have ID 0.
(add-student "Alice" '(93 84 87))
;(add-student "Bob" '(84 71 73))

;(displayln (list-ref student-policies 0))
;(displayln (list-ref student-names 0))
;(displayln (list-ref student-grades 0))
(displayln student-grades)
;(displayln (obs (list-ref student-policies 0) "professor" student-grades))

#|
(display "Alice's grades: ")
(print-grades 1)

(display "Bob's grades: ")
(print-grades 0)
|#


;(display "Class average grade, viewed by Alice: ")
;(print-average-grade "Alice")
