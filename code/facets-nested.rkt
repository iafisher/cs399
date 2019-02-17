#lang reader "racets.rkt"

; Example implementation of nested facet from p. 3 of Racets paper.
(define family-policy (let-label l (lambda (x) (equal? x "family")) l))
(define friend-policy (let-label l (lambda (x) (equal? x "friend")) l))

(define profile (fac family-policy "555-555-5555" (fac friend-policy "soccer" "Alice")))

; This doesn't work, since the facet is protected first by the family policy.
(displayln (obs friend-policy "friend" profile))
(display "\n")

; This also doesn't work, since the inner obs resolves to "555-555-5555" immediately.
(displayln (obs friend-policy "friend" (obs family-policy "family" profile)))
(display "\n")

; This works.
(displayln (obs family-policy "friend" (obs friend-policy "friend" profile)))
