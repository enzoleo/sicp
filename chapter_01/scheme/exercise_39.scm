;; The solution of exercise 1.39
;; A continued fraction representation of the tangent function was
;; published in 1770 by the German mathematician J.H. Lambert:
;;
;;                       x
;;     tan x = ----------------------
;;                        x ^ 2
;;              1 - -----------------
;;                           x ^ 2
;;                   3 - ------------
;;                             x ^ 2
;;                        5 - -------
;;                              ...
;;
;; where x is in radians. Define a procedure `(tan-cf x k)` that computes
;; an approximation to the tangent function based on Lambert's formula.
;; `K` specifies the number of terms to compute, as in exercise 1.37.
;;
;; -------- (above from SICP)
;;

;; The `cont-frac` procedure as a iterative process
(define (cont-frac n d k)
  (define (iter counter result)
    (if (= 0 counter)
        result
        (iter (- counter 1)
              (/ (n counter)
                 (+ (d counter)
                    result)))))
  (iter k 0))

;; The iterative process to rewrite the tangent function using Lambert's
;; formula (called `tan-cf` procedure)
(define (tan-cf x k)
  (let ((numerator (* 1.0 x x)))
    (/ x (+ 1.0
            (cont-frac (lambda (i) numerator)
                       (lambda (i)
                         (if (= 0 (modulo i 2))
                             (+ 1 (* 2 i))
                             (- (+ 1 (* 2 i)))))
                       k)))))

(define (main)
  (display "Load this file and use `tan-cf` procedure.\n")
  (display "Compute tangent function using Lambert's formula.\n")
  (display "Input x and k: ")
  (let ((x (read)) (k (read)))
    (display "[  The exact value   ] ")
    (display (tan x)) (newline)
    (display "[ Result (iterative) ] ")
    (display (tan-cf x k)) (newline)))

(main)


