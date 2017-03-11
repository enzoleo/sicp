;; The solution of exercise 1.15
;; The sine of an angle (specified in radians) can be computed by making
;; use of the approximation: sin x = x + o(x ^ 3) -> sin x = x, if x is
;; sufficiently small, and the trigonometric identity
;;
;;     sin r = 3 * sin (r / 3) - 4 * sin (r / 3) ^ 3
;;
;; to reduce the size of the argument of `sin`. (For purposes of this
;; exercise an angle is considered "sufficiently small" if its magnitude is
;; not greater than 0.1 radians.) These ideas are incorporated in the foll-
;; owing procedures:
;;

;; Use the default exponential function
;; (define (cube x) (* x x x)) is also available in this exercise
(defun cube (x) (expt x 3))
(defun p (x) (- (* 3 x) (* 4 (cube x))))

;; The absolute value of a number
(defun fabs (x)
  (if (< x 0) (- x) x))

;; The new sin function
(defun sine (angle)
  (if (not (> (fabs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;;
;; [1] How many times is the procedure p applied when (sine 12.15) is
;;     evaluated?
;;
;; [2] What is the order of growth in space and number of steps (as a
;;     function of a) used by the process generated by the sine procedure
;;     when (sine a) is evaluated?
;;
;; Use trace function to see number of times the procedure is applied!
;;
;; * (trace sine)
;; 
;; (SINE)
;; * (sine 12.15)
;;   0: (SINE 12.15)
;;     1: (SINE 4.0499997)
;;       2: (SINE 1.3499999)
;;         3: (SINE 0.44999996)
;;           4: (SINE 0.14999999)
;;             5: (SINE 0.049999997)
;;             5: SINE returned 0.049999997
;;           4: SINE returned 0.1495
;;         3: SINE returned 0.43513453
;;       2: SINE returned 0.9758465
;;     1: SINE returned -0.7895632
;;   0: SINE returned -0.39980316
;;; -0.39980316
;; * 
;;
;; The procedure `p` is called 5 times!
;; The time and space complexity are both O(log a)
;;

(defun main ()
  (format t "Compute sine function sine(x) approximately. Input x: ~%")
  (let ((x (read)))
    (format t "[Approximate value] sine(~d) = ~a ~%" x (sine x))))

(main)

