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
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Necessary functions
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (main)
  (display "Load this file and use `compose` procedure.\n")
  (display "EXAMPLE: ((compose square inc) 6)\n")
  (display "RESULT: ")
  (display ((compose square inc) 6))
  (newline))

(main)


