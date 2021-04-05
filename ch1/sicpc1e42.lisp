;; The solution of exercise 1.42
;; Let f and g be two one-argument functions. The composition f after g is
;; defined to be the function x -> f(g(x)). Define a procedure compose
;; that implements composition. For example, if `inc` is a procedure that
;; adds 1 to its argument,
;;
;;     ((compose square inc) 6)
;;     49
;;
;; -------- (above from SICP)
;;

;; Define composition procedure
(defun compose (f g)
  (lambda (x)
    (funcall f (funcall g x))))

;; Necessary functions
(defun inc (x) (+ x 1))
(defun square (x) (* x x))

(defun main ()
  (format t "Load this file and use `compose` procedure. ~%")
  (format t "EXAMPLE: ((compose #'square #'inc) 6) ~%")
  (format t "RESULT: ~a ~%"
          (funcall (compose #'square #'inc) 6)))

(main)


