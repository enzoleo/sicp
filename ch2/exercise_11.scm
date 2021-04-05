;; The solution of exercise 2.11
;; In passing, Ben also cryptically comments: "By testing the signs of
;; the endpoints of the intervals, it is possible to break `mul-interval`
;; into nine cases, only one of which requires more than two
;; multiplications." Rewrite this procedure using Ben's suggestion.
;;
;; -------- (above from SICP)
;;

;; Define intervals
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((lb-x (lower-bound x))
        (ub-x (upper-bound x))
        (lb-y (lower-bound y))
        (ub-y (upper-bound y)))
    (cond ((>= lb-x 0)
           (cond ((>= lb-y 0)
                  (make-interval (* lb-x lb-y)
                                 (* ub-x ub-y)))
                 ((< ub-y 0)
                  (make-interval (* ub-x lb-y)
                                 (* lb-x ub-y)))
                 (else
                  (make-interval (* ub-x lb-y)
                                   (* ub-x ub-y)))))
          ((< ub-x 0)
           (cond ((>= lb-y 0)
                  (make-interval (* lb-x ub-y)
                                 (* ub-x lb-y)))
                 ((< ub-y 0)
                  (make-interval (* ub-x ub-y)
                                 (* lb-x lb-y)))
                 (else
                  (make-interval (* lb-x ub-y)
                                 (* lb-x lb-y)))))
          (else
           (cond ((>= lb-y 0)
                  (make-interval (* lb-x ub-y)
                                 (* ub-x ub-y)))
                 ((< ub-y 0)
                  (make-interval (* ub-x lb-y)
                                 (* lb-x lb-y)))
                 (else
                  (make-interval (min (* ub-x lb-y)
                                      (* lb-x ub-y))
                                 (max (* lb-x lb-y)
                                      (* ub-x ub-y)))))))))

(define (div-interval x y)
  (let ((ub-y (upper-bound y))
        (lb-y (lower-bound y)))
    (if (or (< ub-y 0) (> lb-y 0))
        (mul-interval x
                      (make-interval (/ 1.0 ub-y)
                                     (/ 1.0 lb-y)))
        (display "ERROR: The dividend interval spans zero!\n"))))

(define (test-mul lb-x ub-x lb-y ub-y)
  (mul-interval
   (make-interval lb-x ub-x)
   (make-interval lb-y ub-y)))

;;
;; JUST A TEST:
;;
;; [GUILE]
;;     scheme@(guile-user)> (test-mul 2 3 5 10)
;;     $1 = (10 . 30)
;;     scheme@(guile-user)> (test-mul 2 3 -5 10)
;;     $2 = (-15 . 30)
;;     scheme@(guile-user)> (test-mul 2 3 -10 1)
;;     $3 = (-30 . 3)
;;     scheme@(guile-user)> (test-mul -2 3 5 10)
;;     $4 = (-20 . 30)
;;     scheme@(guile-user)> (test-mul -2 3 -7 10)
;;     $5 = (-21 . 30)
;;     scheme@(guile-user)> (test-mul -2 0.5 -5 10)
;;     $6 = (-20.0 . 10.0)
;;     scheme@(guile-user)> (test-mul -10 5 -5 1)
;;     $7 = (-25 . 50)
;;     scheme@(guile-user)> (test-mul -2 3 -3 -1)
;;     $8 = (-9 . 6)
;;     scheme@(guile-user)> (test-mul -3 -2 1 2)
;;     $9 = (-6 . -2)
;;     scheme@(guile-user)> (test-mul -3 -2 -1 2)
;;     $10 = (-6 . 3)
;;     scheme@(guile-user)> (test-mul -3 -2 -4 -1)
;;     $11 = (2 . 12)
;;
(define (main)
  (display "Load this file and use `make-interval`")
  (newline))

(main)


