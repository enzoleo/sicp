;; The solution of exercise 1.44
;; The idea of smoothing a function is an important concept in signal
;; processing. If f is a function and dx is some small number, then the
;; smoothed version of f is the function whose value at a point x is the
;; average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth
;; that takes as input a procedure that computes f and returns a procedure
;; that computes the smoothed f. It is sometimes valuable to repeatedly
;; smooth a function (that is, smooth the smoothed function, and so on) to
;; obtained the n-fold smoothed function. Show how to generate the `n-fold
;; smoothed function of any given function using `smooth` and `repeated`
;; from exercise 1.43.
;;
;; -------- (above from SICP)
;;

;; Define an infinitely small quantity
(defvar dx 0.00001)

;; Define composition procedure
(defun compose (f g)
  (lambda (x)
    (funcall f (funcall g x))))

;; Necessary functions
(defun self-pow (x) (expt x x))

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

;; Smooth a function
(defun smooth (f)
  (lambda (x)
    (/ (+ (funcall f x)
          (funcall f (+ x dx))
          (funcall f (- x dx)))
       3.0)))

;; Define a n fold smoothed function
(defun nf-smooth (f n)
  (funcall (repeated #'smooth n) f))

(defun main ()
  (format t "Load this file and use `nf-smooth` procedure. ~%")
  (format t "EXAMPLE: (funcall (nf-smooth #'self-pow 5) 5) ~%")
  (format t "RESULT: ~f ~%"
           (funcall (nf-smooth #'self-pow 5) 5)))

(main)


