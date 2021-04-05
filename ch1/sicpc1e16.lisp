;; The solution of exercise 1.16
;; Design a procedure that evolves an iterative exponential process that
;; uses successive squaring and uses a logarithmic number of steps, as does
;; `fast-expt`.
;;
;; [Hint] Using the observation that (b ^ (n / 2)) ^ 2 = (b ^ 2) ^ (n / 2),
;; keep, along with the exponent n and the base b, an additional state
;; variable a, and define the state transformation in such a way that the
;; product a * b ^ n is unchanged from state to state. At the beginning of
;; the process a is taken to be 1, and the answer is given by the value of
;; a at the end of the process. In general, the technique of defining an
;; invariant quantity that remains unchanged from state to state is a
;; powerful way to think about the design of iterative algorithms.
;;
;; -------- (above from SICP)
;;

(defun compute-iter (a x n)
  (cond ((= n 0) a)
        ((= 0 (mod n 2))
         (compute-iter a (* x x) (/ n 2)))
        ((= 1 (mod n 2))
         (compute-iter (* a x) x (- n 1)))))

(defun fast-expt-iter (x n)
  (compute-iter 1 x n))

;;
;; Use trace to see the calls. For example, trace (compute-iter 1 3 37):
;; * (trace compute-iter)
;;
;; (COMPUTE-ITER)
;; * (compute-iter 1 3 37)
;;   0: (COMPUTE-ITER 1 3 37)
;;     1: (COMPUTE-ITER 3 3 36)
;;       2: (COMPUTE-ITER 3 9 18)
;;         3: (COMPUTE-ITER 3 81 9)
;;           4: (COMPUTE-ITER 243 81 8)
;;             5: (COMPUTE-ITER 243 6561 4)
;;               6: (COMPUTE-ITER 243 43046721 2)
;;                 7: (COMPUTE-ITER 243 1853020188851841 1)
;;                   8: (COMPUTE-ITER 450283905890997363 1853020188851841 0)
;;                   8: COMPUTE-ITER returned 450283905890997363
;;                 7: COMPUTE-ITER returned 450283905890997363
;;               6: COMPUTE-ITER returned 450283905890997363
;;             5: COMPUTE-ITER returned 450283905890997363
;;           4: COMPUTE-ITER returned 450283905890997363
;;         3: COMPUTE-ITER returned 450283905890997363
;;       2: COMPUTE-ITER returned 450283905890997363
;;     1: COMPUTE-ITER returned 450283905890997363
;;   0: COMPUTE-ITER returned 450283905890997363
;; 450283905890997363
;;

(defun main ()
  (format t "Compute exponential function x ^ n. Input x, n: ")
  (let ((x (read)) (n (read)))
    (format t "[Iterative process] ~a ^ ~d = ~a ~%"
            x n (fast-expt-iter x n))))

(main)

