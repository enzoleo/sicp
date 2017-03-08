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
(defvar dx 0.00001)

;; The tolerance used in the fixed-point procedure
(defvar tolerance 0.00001)

;; Numerical differentiation
(defun deriv (f)
  (lambda (x)
    (/ (- (funcall f (+ x dx)) (funcall f x))
       dx)))

;; Newton transformation
(defun newton-transform (f)
  (lambda (x)
    (- x (/ (funcall f x) (funcall (deriv f) x))))) 

;; Fix-point computation procedure
(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess step)
    (let ((next (funcall f guess)))
      (format t "Step[~d] ~c ~f ~%" step #\tab next)
      (if (close-enough? guess next)
          next
          (try next (+ 1 step)))))
  (try first-guess 1))

;; Newton's method to search fixed point
(defun newtons-method (f guess)
  (fixed-point (newton-transform f) guess))

;; Define a cubic polynomial function
(defun cubic (a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;; Root a cubic equation x ^ 3 + a * x ^ 2 + b * x + c = 0
(defun cubic-equation-root (a b c)
  (newtons-method (cubic a b c) 1.0))

;;
;; Example: Root equation: x ^ 3 - 3 * x ^ 2 + 5 * x - 6 = 0
;; Input coefficients a, b and c: -3 5 -6
;; [ ROOT PROCESS ]
;; Step[1] 	 2.4979658 
;; Step[2] 	 2.1153245 
;; Step[3] 	 2.0073116 
;; Step[4] 	 1.9997619 
;; Step[5] 	 2.000002 
;; Step[6] 	 2.0 
;; 2.0 
;;

(defun main ()
  (format t "Load this file and use `cubic-equation-root` procedure.~%")
  (format t "Compute a root of one cubic equation from first-guess 1.0.~%")
  (format t "Input coefficients a, b and c: ")
  (let ((a (read)) (b (read)) (c (read)))
    (format t "[ ROOT PROCESS ]~%")
    (format t "~a ~%" (cubic-equation-root a b c))))

(main)


