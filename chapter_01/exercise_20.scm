;; The solution of exercise 1.20
;; The process that a procedure generates is of course dependent on the
;; rules used by the interpreter. As an example, consider the iterative gcd
;; procedure.

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

;;
;; How many `modulo` operations are actually performed in the normal-order
;; evaluation of (gcd 206 40)? In the applicative-order evaluation?
;;
;; Use trace to see the calls. For example, trace (gcd 206 40):
;; trace: (gcd 206 40)
;; trace: (gcd 40 6)
;; trace: (gcd 6 4)
;; trace: (gcd 4 2)
;; trace: (gcd 2 0)
;; trace: 2
;;
;; This in an interpreter using applicative-order. So we know there are 5
;; times `modulo` operations called in the applicative-order evaluation.
;; However it takes much more times `modulo` operations in the normal-order
;; evaluation.
;; 

(define (main)
  (display "Compute gcd(m, n).\nInput m, n: ")
  (let ((m (read)) (n (read)))
    (display "[Greatest Common Divisors] gcd(")
    (display m) (display ", ") (display n) (display ") = ")
    (display (gcd m n))
    (newline)))

(main)


