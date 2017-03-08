;; The solution of exercise 1.39
;; A continued fraction representation of the tangent function was
;; published in 1770 by the German mathematician J.H. Lambert:
;;
;;                       x
;;     tan x = ----------------------
;;                        x ^ 2
;;              1 - -----------------
;;                           x ^ 2
;;                   3 - ------------
;;                             x ^ 2
;;                        5 - -------
;;                              ...
;;
;; where x is in radians. Define a procedure `(tan-cf x k)` that computes
;; an approximation to the tangent function based on Lambert's formula.
;; `K` specifies the number of terms to compute, as in exercise 1.37.
;;
;; -------- (above from SICP)
;;

;; The `cont-frac` procedure as a iterative process
(defun cont-frac (n d k)
  (defun iter (counter result)
    (if (= 0 counter)
        result
        (iter (- counter 1)
              (/ (funcall n counter)
                 (+ (funcall d counter)
                    result)))))
  (iter k 0))

;; The iterative process to rewrite the tangent function using Lambert's
;; formula (called `tan-cf` procedure)
(defun tan-cf (x k)
  (let ((numerator (* 1.0 x x)))
    (/ x (+ 1.0
            (cont-frac (lambda (i) numerator)
                       (lambda (i)
                         (if (= 0 (mod i 2))
                             (+ 1 (* 2 i))
                             (- (+ 1 (* 2 i)))))
                       k)))))

(defun main ()
  (format t "Load this file and use `tan-cf` procedure. ~%")
  (format t "Compute tangent function using Lambert's formula. ~%")
  (format t "Input x and k: ")
  (let ((x (read)) (k (read)))
    (format t "[  The exact value   ] ~a ~%" (tan x))
    (format t "[ Result (iterative) ] ~a ~%" (tan-cf x k))))

(main)


