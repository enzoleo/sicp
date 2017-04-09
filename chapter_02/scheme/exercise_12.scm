;; After debugging her program, Alyssa shows it to a potential user, who
;; complains that her program solves the wrong problem. He wants a program
;; that can deal with numbers represented as a center value and an
;; additive tolerance; for example, he wants to work with intervals such
;; as `3.5 ± 0.15` rather than [3.35, 3.65]. Alyssa returns to her desk
;; and fixes this problem by supplying an alternate constructor and
;; alternate selectors:
;;
;;     (define (make-center-width c w)
;;       (make-interval (- c w) (+ c w)))
;;     (define (center i)
;;       (/ (+ (lower-bound i) (upper-bound i)) 2))
;;     (define (width i)
;;       (/ (- (upper-bound i) (lower-bound i)) 2))
;;
;; Unfortunately, most of Alyssa's users are engineers. Real engineering
;; situations usually involve measurements with only a small uncertainty,
;; measured as the ratio of the width of the interval to the midpoint of
;; the interval. Engineers usually specify percentage tolerances on the
;; parameters of devices, as in the resistor specifications given earlier.
;;
;; The solution of exercise 2.12
;; Define a constructor make-center-percent that takes a center and a
;; percentage tolerance and produces the desired interval. You must also
;; define a selector percent that produces the percentage tolerance for a
;; given interval. The center selector is the same as the one shown above.
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
    (cond ((or (and (>= pct-iy 100) (< pct-ix 100))
               (and (>= pct-ix 100) (> pct-iy pct-ix)))
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

(define (print-real-interval i)
  (display "PRINT INTERVAL: [")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "] \n"))

(define (print-interval i)
  (display "PRINT INTERVAL: ")
  (display (center i))
  (display "(± ")
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
;;     PRINT INTERVAL: [10.000499999999999, 29.99925] 
;;     PRINT INTERVAL: 19.999875(± 49.997187482421765%) 
;;     PRINT INTERVAL: [-15.0, 30.0] 
;;     PRINT INTERVAL: 7.5(± 300.0%) 
;;     PRINT INTERVAL: [-30.000300000000003, -1.9998000000000005] 
;;     PRINT INTERVAL: -16.00005(± 87.50128905847168%) 
;;     PRINT INTERVAL: [-19.9995, 29.99925] 
;;     PRINT INTERVAL: 4.999874999999999(± 500.0000000000001%) 
;;     PRINT INTERVAL: [-20.0, 30.0] 
;;     PRINT INTERVAL: 5.0(± 500.0%) 
;;     PRINT INTERVAL: [-21.000149999999994, 30.000149999999994] 
;;     PRINT INTERVAL: 4.5(± 566.6699999999998%) 
;;     PRINT INTERVAL: [-20.00025, 10.000125000000002] 
;;     PRINT INTERVAL: -5.000062499999999(± 300.0000000000001%) 
;;     PRINT INTERVAL: [-25.0, 50.0] 
;;     PRINT INTERVAL: 12.5(± 300.0%) 
;;     PRINT INTERVAL: [-9.0, 6.0] 
;;     PRINT INTERVAL: -1.5(± 500.0%) 
;;     PRINT INTERVAL: [-5.9998499999999995, -2.0000999999999998] 
;;     PRINT INTERVAL: -3.9999749999999996(± 49.997187482421765%) 
;;     PRINT INTERVAL: [-6.0, 3.0] 
;;     PRINT INTERVAL: -1.5(± 300.0%) 
;;     PRINT INTERVAL: [2.0, 12.000000000000002] 
;;     PRINT INTERVAL: 7.000000000000001(± 71.42857142857143%) 
;;

(define (mul-interval-test-cp)
  (test-mul-cp 2.5 20 7.5 33.33)
  (test-mul-cp 2.5 20 2.5 300)
  (test-mul-cp 2.5 20 -5.5 81.82)
  (test-mul-cp 0.5 500 7.5 33.33)
  (test-mul-cp 0.5 500 2.5 300)
  (test-mul-cp 0.5 500 1.5 566.67)
  (test-mul-cp -0.75 166.67 2.5 300)
  (test-mul-cp -2.5 300 -2 150)
  (test-mul-cp 0.5 500 -2 50)
  (test-mul-cp -2.5 20 1.5 33.33)
  (test-mul-cp -2.5 20 0.5 300)
  (test-mul-cp -2.5 20 -2.5 60))

(define (main)
  (display "Load this file and use `make-interval`.\n")
  (display "Here we test some intervals.\n")
  (display "Use `(mul-interval-test-cp)`.")
  (newline))

(main)


