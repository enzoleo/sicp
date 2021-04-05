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

(define (compute-iter a x n)
  (cond ((= n 0) a)
        ((= 0 (modulo n 2))
         (compute-iter a (* x x) (/ n 2)))
        ((= 1 (modulo n 2))
         (compute-iter (* a x) x (- n 1)))))

(define (fast-expt-iter x n)
  (compute-iter 1 x n))

;;
;; Use trace to see the calls. For example, trace (fast-expt-iter 3 37):
;; trace: (fast-expt-iter 3 37)
;; trace: (compute-iter 1 3 37)
;; trace: (compute-iter 3 3 36)
;; trace: (compute-iter 3 9 18)
;; trace: (compute-iter 3 81 9)
;; trace: (compute-iter 243 81 8)
;; trace: (compute-iter 243 6561 4)
;; trace: (compute-iter 243 43046721 2)
;; trace: (compute-iter 243 1853020188851841 1)
;; trace: (compute-iter 450283905890997363 1853020188851841 0)
;; trace: 450283905890997363
;;

(define (main)
  (display "Compute exponential function x ^ n.\nInput x, n: ")
  (let ((x (read)) (n (read)))
    (display "[Iterative process] ")
    (display x) (display " ^ ") (display n) (display " = ")
    (display (fast-expt-iter x n)))
  (newline))

(main)

