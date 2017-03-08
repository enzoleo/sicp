;; The solution of exercise 1.40
;; Define a procedure cubic that can be used together with the newtons-
;; method procedure in expressions of the form
;;
;;     (newtons-method (cubic a b c) 1)
;;
;; to approximate zeros of the cubic x ^ 3 + a * x ^ 2 + b * x + c.
;;
;; -------- (above from SICP)
;;

;; Define an infinitely small quantity
(define dx 0.00001)

;; The tolerance used in the fixed-point procedure
(define tolerance 0.00001)

;; Numerical differentiation
(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

;; Newton transformation
(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x) ((deriv f) x))))) 

;; Fix-point computation procedure
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (let ((next (f guess)))
      (display "Step[")
      (display step)
      (display "] \t ")
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (+ 1 step)))))
  (try first-guess 1))

;; Newton's method to search fixed point
(define (newtons-method f guess)
  (fixed-point (newton-transform f) guess))

;; Define a cubic polynomial function
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;; Root a cubic equation x ^ 3 + a * x ^ 2 + b * x + c = 0
(define (cubic-equation-root a b c)
  (newtons-method (cubic a b c) 1.0))

;;
;; Example: Root equation: x ^ 3 - 3 * x ^ 2 + 5 * x - 6 = 0
;; Input coefficients a, b and c: -3 5 -6
;; [ ROOT PROCESS ]
;; Step[1] 	 2.4999999999568665
;; Step[2] 	 2.1142876979345435
;; Step[3] 	 2.0073667843043377
;; Step[4] 	 2.000032477787667
;; Step[5] 	 2.0000000008277357
;; Step[6] 	 2.000000000000005
;; 2.000000000000005
;;

(define (main)
  (display "Load this file and use `cubic-equation-root` procedure.\n")
  (display "Compute a root of one cubic equation from first-guess 1.0.\n")
  (display "Input coefficients a, b and c: ")
  (let ((a (read)) (b (read)) (c (read)))
    (display "[ ROOT PROCESS ]\n")
    (display (cubic-equation-root a b c))
    (newline)))

(main)


