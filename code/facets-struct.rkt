#lang reader "racets.rkt"

(struct employee (name position salary))

(define bob-policy (let-label l (lambda (x) (equal? x "Bob")) l))
(define bob (employee "Bob" "manager" (fac bob-policy 70000 0)))

; Both work.
(displayln (employee-salary (obs bob-policy "Bob" bob)))
(displayln (obs bob-policy "Bob" (employee-salary bob)))
