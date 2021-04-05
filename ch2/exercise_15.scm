;; The solution of exercise 2.15
;; Eva Lu Ator, another user, has also noticed the different intervals
;; computed by different but algebraically equivalent expressions. She
;; says that a formula to compute with intervals using Alyssa's system
;; will produce tighter error bounds if it can be written in such a form
;; that no variable that represents an uncertain number is repeated. Thus,
;; she says, par2 is a "better" program for parallel resistances than par1.
;; Is she right? Why?
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

;;
;; Eva is right. From the formulas of multiplication and addition, we
;; know that multiplication and division cause larger errors than
;; addition and subtraction do. For example, under the assumption of
;; small percentage tolerances, If Resistance R1 has @percent p1 and R2
;; has @percent p2 >= p1, the function `par1` causes (p1 + 3 * p2) error
;; while the function `par2` only causes p2 error.
;;

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

(define (main)
  (display "Load this file and use `par1` and `par2`.\n")
  (display "Let ix = 10 (± 1%) and iy = 30 (± 2%).\n")
  (display "Test (div-interval ix ix) and (div-interval ix iy).")
  (newline))

(main)


