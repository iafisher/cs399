; A prototype forum application to demonstrate the use of faceted execution.
;
; There are three levels of access to the forum: unauthenticated users can see
; posts authenticated users can see the upvote/downvote score of posts and admins
; can see whether a post has been flagged. Admins and users with more than 100
; karma can see the list of all users known to the forum server.
;
; In order to run this module, you need to copy racets.rkt, racets-mlang.rkt, and
; facets.rkt from https://github.com/fordsec/racets, and place them in the same
; directory as this file.
#lang reader "racets.rkt"


(struct User (name policy karma is-admin))
(struct Post (user text score flagged))


(define user-list-policy
  (let-label l
	     (lambda (username)
	       (let ([user (get-user username)])
		 (or (User-is-admin user) (>= (User-karma user) 100))))
	     l))

; Make a policy for a particular user.
(define (make-policy username)
  (let-label l
	     (lambda (x)
	       (or (equal? x username) (User-is-admin (get-user username)) (equal? x "root")))
	     l))


; Some sample users.
(define alice (User "Alice" (make-policy "Alice") 2000 #f))
(define bob (User "Bob" (make-policy "Bob") 50 #t))
(define eve (User "Eve" (make-policy "Eve") 12 #f))


(define users (fac user-list-policy (list alice bob eve) '()))
(define posts '())


(define (get-user username)
  (define (get-user-rec username lst)
    (if (empty? lst)
      #f
      (if (equal? username (car lst))
	(car lst)
	(get-user-rec username (cdr lst)))))
  (get-user-rec (obs user-list-policy "root" users)))


;;; The mock API endpoints. ;;;
(define (print-post username post-id)
  (displayln "Not implemented yet!"))
