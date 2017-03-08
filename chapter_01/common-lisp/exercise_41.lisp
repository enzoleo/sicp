;; The solution of exercise 1.41
;; Define a procedure `dbl` that takes a procedure of one argument as
;; argument and returns a procedure that applies the original procedure
;; twice. For example, if `inc` is a procedure that adds 1 to its argument,
;; then `(double inc)` should be a procedure that adds 2. What value is
;; returned by
;;
;;     (((dbl (dbl dbl)) inc) 5)
;;
;; -------- (above from SICP)
;;

;; Notice that procedure `f` only has one argument
(defun dbl (f)
  (lambda (x)
    (funcall f (funcall f x))))

;; Increase one number by 1
(defun inc (x) (+ x 1))

;;
;; The result is 21. How to explain?
;; `double` applies the original procedure twice, so `(double double)`
;; actually applies `double` twice, which means, it applies the argument
;; four times. Thus, procedure `(double (double double))` applies the
;; procedure `(double double)` twice, which means: if the original
;; argument is procedure `f`, the following expressions are equivalent:
;;
;;     - ((dbl (dbl dbl)) f)
;;     - ((dbl dbl) ((dbl dbl) f))
;;     - ((dbl dbl) (dbl (dbl f)))
;;     - (dbl (dbl (dbl (dbl f))))
;;
;; Here you should know `f` is applied 16 times.
;;

(defun main ()
  (format t "Load this file and use `double` procedure. ~%")
  (format t "RESULT: ~a ~%"
          (funcall (funcall (dbl (dbl #'dbl)) #'inc) 5)))

(main)


