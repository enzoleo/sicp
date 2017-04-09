;; The solution of exercise 2.13
;; Show that under the assumption of small percentage tolerances there is
;; a simple formula for the approximate percentage tolerance of the
;; product of two intervals in terms of the tolerances of the factors. You
;; may simplify the problem by assuming that all numbers are positive.
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

;; The new multiplication procedure is defined here, under the assumption
;; of small percentage tolerances .
(define (mul-interval-cp x y)
  (let ((ctr-ix (center x))
        (pct-ix (percent x))
        (ctr-iy (center y))
        (pct-iy (percent y)))
    (make-center-percent (* ctr-ix ctr-iy)
                         (+ pct-ix pct-iy))))

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
;; JUST A TEST:
;;
;; [GUILE]
;;     scheme@(guile-user)> (mul-interval-test-cp)
;;     PRINT INTERVAL: [1865.625, 1884.375] 
;;     PRINT INTERVAL: 1875.0 (± 0.5%) 
;;     PRINT INTERVAL: [621.875, 628.125] 
;;     PRINT INTERVAL: 625.0 (± 0.5%) 
;;     PRINT INTERVAL: [-1380.5, -1369.5] 
;;     PRINT INTERVAL: -1375.0 (± 0.4%) 
;;     PRINT INTERVAL: [4092.0000000000005, 4158.0] 
;;     PRINT INTERVAL: 4125.0 (± 0.7999999999999945%) 
;;     PRINT INTERVAL: [1364.0, 1386.0] 
;;     PRINT INTERVAL: 1375.0 (± 0.8%) 
;;     PRINT INTERVAL: [4628.25, 4721.75] 
;;     PRINT INTERVAL: 4675.0 (± 1.0%) 
;;     PRINT INTERVAL: [-1883.625, -1866.375] 
;;     PRINT INTERVAL: -1875.0 (± 0.45999999999999996%) 
;;     PRINT INTERVAL: [1493.25, 1506.75] 
;;     PRINT INTERVAL: 1500.0 (± 0.44999999999999996%) 
;;     PRINT INTERVAL: [-1666.4999999999998, -1633.5000000000002] 
;;     PRINT INTERVAL: -1650.0 (± 0.9999999999999861%) 
;;     PRINT INTERVAL: [-1884.375, -1865.625] 
;;     PRINT INTERVAL: -1875.0 (± 0.5%) 
;;     PRINT INTERVAL: [-1381.875, -1368.125] 
;;     PRINT INTERVAL: -1375.0 (± 0.5%) 
;;     PRINT INTERVAL: [622.5, 627.5] 
;;     PRINT INTERVAL: 625.0 (± 0.4%)
;;

(define (mul-interval-test-cp)
  (test-mul-cp 25 0.2 75 0.3)
  (test-mul-cp 25 0.2 25 0.3)
  (test-mul-cp 25 0.2 -55 0.2)
  (test-mul-cp 55 0.5 75 0.3)
  (test-mul-cp 55 0.5 25 0.3)
  (test-mul-cp 55 0.5 85 0.5)
  (test-mul-cp -75 0.16 25 0.3)
  (test-mul-cp -25 0.3 -60 0.15)
  (test-mul-cp 55 0.5 -30 0.5)
  (test-mul-cp -25 0.2 75 0.3)
  (test-mul-cp -25 0.2 55 0.3)
  (test-mul-cp -25 0.2 -25 0.2))

(define (main)
  (display "Load this file and use `make-interval`.\n")
  (display "Here we test some intervals.\n")
  (display "Use `(mul-interval-test-cp)`.")
  (newline))

(main)


