;; The solution of exercise 2.6
;; In case representing pairs as procedures wasn't mind-boggling enough,
;; consider that, in a language that can manipulate procedures, we can get
;; by without numbers (at least insofar as nonnegative integers are
;; concerned) by implementing 0 and the operation of adding 1 as
;;
;;     (defun zero (f) (lambda (x) x))
;;     (defun add-1 (n)
;;       (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))
;;
;; This representation is known as Church numerals, after its inventor,
;; Alonzo Church, the logician who invented the calculus.
;;
;; Define one and two directly (not in terms of zero and add-1). (Hint:
;; Use substitution to evaluate `(add-1 zero)`). Give a direct definition
;; of the addition procedure `+` (not in terms of repeated application of
;; `add-1`).
;;
;; -------- (above from SICP)
;;

;; The new-defined `numbers` in Church numerals
(defun zero (f) (lambda (x) x))
(defun one (f) (lambda (x) (funcall f x)))
(defun two (f) (lambda (x) (funcall f (funcall f x))))

(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))

(defun church-plus (m n)
  (lambda (f)
    (lambda (x)
      (funcall (funcall m f) (funcall (funcall n f) x)))))

;;
;; [SBCL] Load this file:
;;
;;     (load "exercise_06.lisp")
;;
;; Define a function for test (here `double` is defined):
;;
;;     (defun dbl (x) (+ x x))
;;
;; Test our procedures defined above:
;;
;;     (funcall (zero #'dbl) 1)                               1
;;     (funcall (one #'dbl) 1)                                2
;;     (funcall (two #'dbl) 1)                                4
;;     (funcall (funcall (add-1 #'two) #'dbl) 1)              8
;;     (funcall (funcall (church-plus #'one #'two) #'dbl) 1)  8
;;

(defun main ()
  (format t "Load this file and use `church-plus`.~%"))

(main)


