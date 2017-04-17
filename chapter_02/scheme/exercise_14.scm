;; After considerable work, Alyssa P. Hacker delivers her finished system.
;; Several years later, after she has forgotten all about it, she gets a
;; frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has
;; noticed that the formula for parallel resistors can be written in two
;; algebraically equivalent ways:
;;
;;      R_1 * R_2                         1
;;     -----------      and      -------------------
;;      R_1 + R_2                 1 / R_1 + 1 / R_2
;;
;; He has written the following two programs, each of which computes the
;; parallel-resistors formula differently:
;;
;;     (define (par1 r1 r2)
;;         (div-interval (mul-interval r1 r2)
;;                       (add-interval r1 r2)))
;;     (define (par2 r1 r2)
;;         (let ((one (make-interval 1 1)))
;;           (div-interval one
;;                         (add-interval (div-interval one r1)
;;                                       (div-interval one r2)))))
;;
;; Lem complains that Alyssa's program gives different answers for the two
;; ways of computing. This is a serious complaint.
;;
;; The solution of exercise 2.14
;; Demonstrate that Lem is right. Investigate the behavior of the system
;; on a variety of arithmetic expressions. Make some intervals A and B,
;; and use them in computing the expressions A / A and A / B. You will get
;; the most insight by using intervals whose width is a small percentage of
;; the center value. Examine the results of the computation in center-
;; percent form (see exercise 2.12).
;;
;; -------- (above from SICP)
;;

;; Define intervals
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;; Make an interval with its center and width
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

;; Return the center of an interval
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;; Return the width of an interval
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Make an interval with its center and percentage tolerance
(define (make-center-percent c p)
  (let ((w (abs (* c p 0.01))))
    (make-interval (- c w) (+ c w))))

;; Return the percentage tolerance of an interval
(define (percent i)
  (* 100.0
     (abs (/ (width i) (center i)))))

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

(define (mul-interval-cp x y)
  (let ((ctr-ix (center x))
        (pct-ix (percent x))
        (ctr-iy (center y))
        (pct-iy (percent y)))
    (cond ((and (>= pct-iy 100) (> pct-iy pct-ix))
           (make-center-percent (* (+ 1 (/ pct-ix 100.0))
                                   ctr-ix ctr-iy)
                                pct-iy))
          ((and (< pct-ix 100)
                (< pct-iy 100))
           (make-center-percent (* (+ 1 (/ (* pct-ix pct-iy)
                                           10000.0))
                                   ctr-ix ctr-iy)
                                (/ (* (+ pct-ix pct-iy)
                                      10000.0)
                                   (+ (* pct-ix
                                         pct-iy) 10000))))
          (else
           (make-center-percent (* (+ 1 (/ pct-iy 100.0))
                                   ctr-ix ctr-iy)
                                pct-ix)))))

(define (div-interval x y)
  (let ((ub-y (upper-bound y))
        (lb-y (lower-bound y)))
    (if (or (< ub-y 0) (> lb-y 0))
        (mul-interval x
                      (make-interval (/ 1.0 ub-y)
                                     (/ 1.0 lb-y)))
        (display "ERROR: The dividend interval spans zero!\n"))))

(define (div-interval-cp x y)
  (let ((ctr-y (center y))
        (pct-y (percent y)))
    (if (< pct-y 100)
        (mul-interval-cp x
                         (make-center-percent
                          (/ (/ 10000.0 ctr-y)
                             (- 10000 (* pct-y pct-y)))
                          pct-y))
        (display "ERROR: The dividend interval spans zero!\n"))))

(define (par1 r1 r2)
  (div-interval-cp (mul-interval-cp r1 r2)
                   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval-cp one
                     (add-interval (div-interval-cp one r1)
                                   (div-interval-cp one r2)))))

(define (print-real-interval i)
  (display "PRINT INTERVAL: [")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "] \n"))

(define (print-interval i)
  (display "PRINT INTERVAL: ")
  (display (center i))
  (display " (± ")
  (display (percent i))
  (display "%) \n"))

(define (test-mul lb-x ub-x lb-y ub-y)
  (mul-interval
   (make-interval lb-x ub-x)
   (make-interval lb-y ub-y)))

(define (test-mul-cp ctr-x pct-x ctr-y pct-y)
  (let ((tmp-mul
         (mul-interval-cp (make-center-percent ctr-x pct-x)
                          (make-center-percent ctr-y pct-y))))
    (print-real-interval tmp-mul)
    (print-interval tmp-mul)))

;;
;; [GUILE]
;;     scheme@(guile-user)> (define ix (make-center-percent 10 1))
;;     scheme@(guile-user)> (define iy (make-center-percent 30 2))
;;
;;     scheme@(guile-user)> (print-interval (div-interval-cp ix ix))
;;     PRINT INTERVAL: 1.000200020002 (± 1.9998000199979857%)
;;
;;     scheme@(guile-user)> (print-interval (div-interval-cp ix iy))
;;     PRINT INTERVAL: 0.33353341336534603 (± 2.9994001199760025%) 
;;
;;     scheme@(guile-user)> (define iz1 (par1 ix iy))
;;     scheme@(guile-user)> (define iz2 (par2 ix iy))
;;
;;     scheme@(guile-user)> (print-interval iz1)
;;     PRINT INTERVAL: 7.507736744377964 (± 4.746908491343787%) 
;;
;;     scheme@(guile-user)> (print-interval iz2)
;;     PRINT INTERVAL: 7.499859331920401 (± 1.250056268287181%)
;;

(define (main)
  (display "Load this file and use `par1` and `par2`.\n")
  (display "Let ix = 10 (± 1%) and iy = 30 (± 2%).\n")
  (display "Test (div-interval ix ix) and (div-interval ix iy).")
  (newline))

(main)


