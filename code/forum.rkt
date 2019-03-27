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
(struct Post (username text score flagged))


; A policy for access to the list of users.
(define user-list-policy
  (let-label l
             (lambda (username)
               (if (equal? username "root")
                 #t
                 (let ([user (get-user username)])
                   (and user (or (User-is-admin user) (>= (User-karma user) 100))))))
             l))

; A policy for data that any admin can access.
(define admin-policy
  (let-label l
             (lambda (username)
               (if (equal? username "root")
                 #t
                 (let ([user (get-user username)])
                   (and user (User-is-admin user)))))
             l))

; A policy for data that any user can access.
(define user-policy
  (let-label l
             (lambda (username) (or (equal? username "root") (get-user username)))
             l))


; Make a policy for a particular user.
(define (make-policy username)
  (let-label l
             (lambda (x)
               (or (equal? x username) (User-is-admin (get-user username)) (equal? x "root")))
             l))


; Helper function to create a user.
(define (make-user name karma is-admin) (User name (make-policy name) karma is-admin))

; Helper function to create a post.
(define (make-post username text score flagged)
  (fac admin-policy
       (Post username text score flagged)
       (fac user-policy
            (Post username text score #f)
            (Post "" "" 0 #f))))


; Convert a Post object to a string.
(define (render-post post)
  (string-append
    "(Post " 
    (~s (Post-username post))
    " "
    (~s (Post-text post))
    " "
    (~a (Post-score post))
    " "
    (~a (Post-flagged post))
    ")"))


; Convert a User object to a string.
(define (render-user user)
  (string-append
    "(User "
    (~s (User-name user))
    " <policy> "
    (~a (User-karma user))
    " "
    (~a (User-is-admin user))
    ")"))


; Some sample users.
(define alice (make-user "Alice" 2000 #f))
(define bob (make-user "Bob" 50 #t))
(define eve (make-user "Eve" 12 #f))


(define users (fac user-list-policy (list alice bob eve) '()))
(define posts (list (make-post "Alice" "Hello everybody!" 23 #t)))


(define (get-user username)
  (define (get-user-rec username lst)
    (if (empty? lst)
      #f
      (if (equal? username (User-name (car lst)))
        (car lst)
        (get-user-rec username (cdr lst)))))
  (get-user-rec username (obs user-list-policy "root" users)))


;;; The mock API endpoints. ;;;
(define (print-post username post-id)
  (displayln
    (render-post
      (list-ref
        (obs user-policy username (obs admin-policy username posts))
        post-id))))

(define (print-user-list username)
  (displayln (map render-user (obs user-list-policy username users))))

(define (post-to-forum username text)
  (set! posts (cons (make-post username text 0 #f) posts)))


; Some sample API calls.
(display "User list, observed by Alice: ")
(print-user-list "Alice")
(display "User list, observed by Bob: ")
(print-user-list "Bob")
(display "User list, observed by Eve: ")
(print-user-list "Eve")
(display "\n")

(display "Post 0, observed by Alice: ")
(print-post "Alice" 0)
(display "Post 0, observed by Bob: ")
(print-post "Bob" 0)
(display "Post 0, observed by Eve: ")
(print-post "Eve" 0)
(display "Post 0, observed by stranger: ")
(print-post "stranger" 0)
(display "\n")

(displayln "Making a new post...\n")
(post-to-forum "Eve" "Hi Alice!")

; Note that since posts are prepended to the list, Post 0 is now at index 1 and
; Post 1 is at index 0.
(display "Post 1, observed by Alice: ")
(print-post "Alice" 0)
(display "Post 0 again, observed by Alice: ")
(print-post "Alice" 1)
