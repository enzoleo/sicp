;; The solution of exercise 2.10
;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's
;; shoulder and comments that it is not clear what it means to divide by
;; an interval that spans zero. Modify Alyssa's code to check for this
;; condition and to signal an error if it occurs.
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
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((ub-y (upper-bound y))
        (lb-y (lower-bound y)))
    (if (or (< ub-y 0) (> lb-y 0))
        (mul-interval x
                      (make-interval (/ 1.0 ub-y)
                                     (/ 1.0 lb-y)))
        (display "ERROR: The dividend interval spans zero!\n"))))

(define (main)
  (display "Load this file and use `make-interval`")
  (newline))

(main)


