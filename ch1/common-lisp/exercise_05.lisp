;; The solution of exercise 1.5
;; What behavior will you observe with an interpreter that uses
;; applicative-order evaluation? What behavior will you observe with
;; an interpreter that uses normal-order evaluation?
;;
;; Assume that the evaluation rule for the special form `if` is the
;; same whether the interpreter is using normal or applicative order:
;; The predicate expression is evaluated first, and the result determines
;; whether to evaluate the consequent or the alternative expression.

;; Define a procedure named `p`.
;; Notice this procedure has no formal parameters in its definition, and
;; the body actually does nothing. So this procedure will call itself
;; again and again without an end.
(defun p () (p))

(defun test (x y)
  (if (= x 0)
      0
      y))

;; ATTENTION: The interpreter using normal order will return 0, while the
;; interpreter using applicatice order could not return any result. You
;; have to kill the procedure by hand. The most probable result:
;;  
;; SBCL:
;;     *** - Program stack overflow. RESET
;;

(defun main ()
  (format t "Result: ~a ~%" (test 0 (p))))

(main)

