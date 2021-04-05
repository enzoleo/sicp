;; The solution of exercise 1.41
;; Define a procedure `double` that takes a procedure of one argument as
;; argument and returns a procedure that applies the original procedure
;; twice. For example, if `inc` is a procedure that adds 1 to its argument,
;; then `(double inc)` should be a procedure that adds 2. What value is
;; returned by
;;
;;     (((double (double double)) inc) 5)
;;
;; -------- (above from SICP)
;;

;; Notice that procedure `f` only has one argument
(define (double f)
  (lambda (x)
    (f (f x))))

;; Increase one number by 1
(define (inc x) (+ x 1))

;;
;; The result is 21. How to explain?
;; `double` applies the original procedure twice, so `(double double)`
;; actually applies `double` twice, which means, it applies the argument
;; four times. Thus, procedure `(double (double double))` applies the
;; procedure `(double double)` twice, which means: if the original
;; argument is procedure `f`, the following expressions are equivalent:
;;
;;     - ((double (double double)) f)
;;     - ((double double) ((double double) f))
;;     - ((double double) (double (double f)))
;;     - (double (double (double (double f))))
;;
;; Here you should know `f` is applied 16 times.
;;

(define (main)
  (display "Load this file and use `double` procedure.\n")
  (display "RESULT: ")
  (display (((double (double double)) inc) 5))
  (newline))

(main)


