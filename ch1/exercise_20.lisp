;; The solution of exercise 1.20
;; The process that a procedure generates is of course dependent on the
;; rules used by the interpreter. As an example, consider the iterative gcd
;; procedure.

(defun gc-divisor (a b)
  (if (= b 0)
      a
      (gc-divisor b (mod a b))))

;;
;; How many `modulo` operations are actually performed in the normal-order
;; evaluation of (gcd 206 40)? In the applicative-order evaluation?
;;
;; Use trace to see the calls. For example, trace (gcd 206 40):
;; * (trace gc-divisor)
;;
;; (GC-DIVISOR)
;; * (gc-divisor 206 40)
;;   0: (GC-DIVISOR 206 40)
;;     1: (GC-DIVISOR 40 6)
;;       2: (GC-DIVISOR 6 4)
;;         3: (GC-DIVISOR 4 2)
;;           4: (GC-DIVISOR 2 0)
;;           4: GC-DIVISOR returned 2
;;         3: GC-DIVISOR returned 2
;;       2: GC-DIVISOR returned 2
;;     1: GC-DIVISOR returned 2
;;   0: GC-DIVISOR returned 2
;; 2
;;
;; This in an interpreter using applicative-order. So we know there are 5
;; times `modulo` operations called in the applicative-order evaluation.
;; However it takes much more times `modulo` operations in the normal-order
;; evaluation.
;; 

(defun main ()
  (format t "Load this file and use `gc-divisor` function. ~%")
  (format t "Compute gcd(m, n).~%Input m, n: ")
  (let ((m (read)) (n (read)))
    (format t "[Greatest Common Divisors] gcd(~d, ~d) = ~d ~%"
            m n (gc-divisor m n))))

(main)


