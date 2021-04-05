;; The solution of exercise 1.19
;; There is a clever algorithm for computing the Fibonacci numbers in a
;; logarithmic number of steps. Recall the transformation of the state
;; variables a and b in the `fib-iter` process of section 1.2.2:
;;
;;     a <- a + b,   and   b <- a
;;
;; Call this transformation T, and observe that applying T over and over
;; again n times, starting with 1 and 0, produces the pair Fib(n + 1) and
;; Fib(n). In other words, the Fibonacci numbers are produced by applying
;; T ^ n, the n th power of the transformation T, starting with the pair
;; (1, 0). Now consider T to be the special case of p = 0 and q = 1 in a
;; family of transformations T_{pq}, where T_{pq} transforms the pair
;; (a, b) according to
;;
;;     a <- b * q + a * q + a * p,   and   b <- b * p + a * q
;;
;; Show that if we apply such a transformation T_{pq} twice, the effect is
;; the same as using a single transformation T_{p'q'} of the same form, and
;; compute p' and q' in terms of p and q. This gives us an explicit way to
;; square these transformations, and thus we can compute T ^ n using
;; successive squaring, as in the `fast-expt` procedure.
;;
;; -------- (above from SICP)
;;
;; Actually we can describe the basic thought in mathematical languages.
;; Assume we have a constant matrix:
;;
;;         / 1  1 \                          / 1 \
;;     T = |      |  , and init vector:  v = |   |
;;         \ 1  0 /                          \ 0 /
;;
;; Then, when n >= 2, obviously we have:
;;
;;     /   Fib(n)   \   / 1  1 \ / Fib(n - 1) \
;;     |            | = |      | |            | = ... T ^ (n - 1) * v
;;     \ Fib(n - 1) /   \ 1  0 / \ Fib(n - 2) /
;;
;; Generally, we set matrix:
;;
;;              / p + q  q \
;;     T_{pq} = |          |  , then T = T_{01}
;;              \   q    p /
;;
;;                  / p + q  q \ / p + q  q \
;;     T_{pq} ^ 2 = |          | |          | = |
;;                  \   q    p / \   q    p /
;;
;;                  / (p + q) ^ 2 + q ^ 2   (p + q) * q + q * p \
;;                = |                                           |
;;                  \ (p + q) * q + q * p       q ^ 2 + p ^ 2   /
;;
;;                  / p' + q'  q' \          / p' \   /   q ^ 2 + p ^ 2   \
;;                = |             | ,  where |    | = |                   |
;;                  \    q'    p' /          \ q' /   \ q ^ 2 + q * p * 2 /
;;
;; Now we realize the algorithm in scheme.
;;

(defun fib-iter (a b p q count)
  (cond ((= count 0) b)
        ((= 0 (mod count 2))
         (fib-iter a
                   b
                   (+ (* q q) (* p p))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (t (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1)))))

(defun fib (n)
  (fib-iter 1 0 0 1 n))

;; List the Fibonacci numbers
(defun display-iter (n)
  (do ((counter 0 (1+ counter)))
      ((> counter n))
    (format t "fibonacci(~d) ~c~d ~%"
            counter #\tab (fib counter))))

;;
;; Use trace to see the calls. For example, trace (fib 60):
;; * (trace fib-iter)
;;
;; (FIB-ITER)
;; * (fib-iter 1 0 0 1 60)
;;   0: (FIB-ITER 1 0 0 1 60)
;;     1: (FIB-ITER 1 0 1 1 30)
;;       2: (FIB-ITER 1 0 2 3 15)
;;         3: (FIB-ITER 5 3 2 3 14)
;;           4: (FIB-ITER 5 3 13 21 7)
;;             5: (FIB-ITER 233 144 13 21 6)
;;               6: (FIB-ITER 233 144 610 987 3)
;;                 7: (FIB-ITER 514229 317811 610 987 2)
;;                   8: (FIB-ITER 514229 317811 1346269 2178309 1)
;;                     9: (FIB-ITER 2504730781961 1548008755920 1346269 2178309 0)
;;                     9: FIB-ITER returned 1548008755920
;;                   8: FIB-ITER returned 1548008755920
;;                 7: FIB-ITER returned 1548008755920
;;               6: FIB-ITER returned 1548008755920
;;             5: FIB-ITER returned 1548008755920
;;           4: FIB-ITER returned 1548008755920
;;         3: FIB-ITER returned 1548008755920
;;       2: FIB-ITER returned 1548008755920
;;     1: FIB-ITER returned 1548008755920
;;   0: FIB-ITER returned 1548008755920
;; 1548008755920
;;

(defun main ()
  (format t "Compute Fibonacci number f(n).~%Input n: ")
  (let ((n (read)))
    (format t "[Fibonacci numbers]~%")
    (display-iter n)))

(main)


