;; The solution of exercise 1.10
;; The following procedure computes a mathematical function called
;; Ackermann's function.
;;

(defun ackermann (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (ackermann (- x 1)
                         (ackermann x (- y 1))))))

;;
;; What are the values of the following expressions?
;;
;; (ackermann 1 10)
;; (ackermann 2 4)
;; (ackermann 3 3)
;;
;; According to the given formulas, these expressions are really simple
;; to compute. But here we still compute them in scheme.
;;

(defun print-expressions ()
  (format t "~a~%~a~%~a~%"
          (ackermann 1 10)
          (ackermann 2 4)
          (ackermann 3 3)))

;; Consider the following procedures, where `ackermann` is the procedure
;; defined above:
(defun f (n) (ackermann 0 n))
(defun g (n) (ackermann 1 n))
(defun h (n) (ackermann 2 n))

;;
;; Give concise mathematical definitions for the functions computed by the
;; procedure f, g and h for positive integer values of n. For example,
;; the function k defined as following procedure:
;;
;;     (defun k (n) (* 5 n n))
;;
;; computes the mathematical formula 5n^2.
;; You can get deduce the mathematical formula the functions f, g and h
;; give merely from the definition of ackermann recursion:
;;
;;     f(n) = 2 * n
;;     g(n) = 2 ^ n (n >= 1), g(0) = 0
;;     h(n) = 2 ^ h(n - 1) (n >= 2), h(1) = 2, h(0) = 0
;;
;; Compute some examples to check.
;;

(defun compute-fun (function n)
  (defun output (num)
    (if (< num n)
        (progn
          (format t "func(~a)~C = ~a ~%" num #\tab (funcall function num))
          (output (+ num 1)))))
  (output 0))

(defun main ()
  (format t "The results of the given expressions: ~%")
  (print-expressions)
  (format t "Check the results of function f: ~%")
  (compute-fun #'f 10)
  (format t "Check the results of function g: ~%")
  (compute-fun #'g 10)
  (format t "Check the results of function h: ~%")
  (compute-fun #'h 5))

;;
;; The deduction process is quitee simple:
;;
;;                        | 2 * y,     if x = 0 or y = 0 or y = 1
;;     ackermann(x, y) = <  
;;                        | ackermann(x - 1, ackermann(x, y - 1)), else
;;
;; We compute f(n) first: f(n) = ackermann(0, n) = 2 * n.
;; Then from the formula g(n) = ackermann(1, n), we know:
;;
;;     g(n) = ackermann(0, ackermann(1, n - 1))
;;          = 2 * ackermann(1, n - 1)
;;          = 2 * g(n - 1)                          (n >= 2)
;;
;; and apparently we have:
;;
;;     g(0) = ackermann(1, 0) = 0;
;;     g(1) = ackermann(1, 1) = 2.
;;
;; So we can get: g(n) = 2 ^ n (n >= 1), g(0) = 0.
;; From the formula h(n) = ackermann(2, n), we know:
;;
;;     h(n) = ackermann(1, ackermann(2, n - 1))
;;          = 2 ^ ackermann(2, n - 1)
;;          = 2 ^ h(n - 1)                          (n >= 2)
;;
;; and apparently we have:
;;
;;     h(0) = ackermann(2, 0) = 0;
;;     h(1) = ackermann(2, 1) = 2.
;;
;; Thus we get the formula of sequences h.
;;

(main)


