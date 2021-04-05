;; The solution of exercise 2.4
;; Here is an alternative procedural representation of pairs. For this
;; representation, verify that (new-car (new-cons x y)) yields x for any
;; objects x and y.
;;
;;     (defun new-cons (x y)
;;       (lambda (m) (funcall m x y)))
;;     (defun new-car (z)
;;       (funcall z (lambda (p q) p)))
;;     (defun new-cdr (z)
;;       (funcall z (lambda (p q) q)))
;;
;; What is the corresponding definition of cdr?
;;
;; -------- (above from SICP)
;;

(defun new-cons (x y)
  (lambda (m) (funcall m x y)))
(defun new-car (z)
  (funcall z (lambda (p q) p)))
(defun new-cdr (z)
  (funcall z (lambda (p q) q)))


