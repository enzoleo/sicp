;; The solution of exercise 1.43
;; If f is a numerical function and n is a positive integer, then we can
;; form the n th repeated application of f, which is defined to be the
;; function whose value at x is f(f(...(f(x))...)).
;;
;; For example, if f is the function x -> x + 1, then the nth repeated
;; application of f is the function x -> x + n. If f is the operation of
;; squaring a number, then the nth repeated application of f is the
;; function that raises its argument to the 2 ^ n th power. Write a
;; procedure that takes as inputs a procedure that computes f and a
;; positive integer n and returns the procedure that computes the n th
;; repeated application of f. Your procedure should be able to be used as
;; follows:
;;
;;     (funcall (repeated #'square 2) 5)
;;     625
;;
;; Hint: You may find it convenient to use compose from exercise 1.42.
;;
;; -------- (above from SICP)
;;

;; Define composition procedure
(defun compose (f g)
  (lambda (x)
    (funcall f (funcall g x))))

;; Necessary functions
(defun square (x) (* x x))

;;
;; Repeated function. Actually this procedure is an iterative process. You
;; can compare the efficiency with the following procedure here:
;;
;;     (defun repeated (f n)
;;       (defun iter (counter result)
;;         (if (= counter n)
;;             result
;;             (iter (1+ counter) (funcall f result))))
;;       (lambda (x)
;;         (iter 0 x)))
;;
;; But there exists an interesting thing:
;;
;;     (compose (repeated f (- n 1)) f) [ iterative process ]
;;     (compose f (repeated f (- n 1))) [ recursive process ]
;;
;; It is very easy to explain: the process type is determind by the
;; `compose` procedure.
;;

(defun repeated (f n)
  (lambda (x)
    (cond ((= n 0) x)
          ((= n 1) (funcall f x))
          (t (funcall (compose (repeated f (- n 1)) f) x)))))

(defun main ()
  (format t "Load this file and use `repeated` procedure. ~%")
  (format t "EXAMPLE: (funcall (repeated #'square 2) 5) ~%")
  (format t "RESULT: ~a ~%"
          (funcall (repeated #'square 2) 5)))

(main)


