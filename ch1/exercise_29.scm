;; The solution of exercise 1.29
;; Simpson's Rule is a more accurate method of numerical integration than
;; the method illustrated above. Using Simpson's Rule, the integral of a
;; function f between a and b is approximated as
;;
;;     (h / 3) * [y_0 + 4 * y_1 + 2 * y_2
;;                    + 4 * y_3 + 2 * y_4
;;                    + 4 * y_5 + 2 * y_6
;;                    +   ...   +   ...
;;                    + 4 * y_{n - 2} + 2 * y_{n - 1} + y_n ]
;;
;; where h = (b - a) / n, for some even integer n, and y_k = f(a + k * h).
;; (Increasing n increases the accuracy of the approximation.) Define a
;; procedure that takes as arguments f, a, b, and n and returns the value
;; of the integral, computed using Simpson's Rule. Use your procedure to
;; integrate `cube` between 0 and 1 (with n = 100 and n = 1000), and
;; compare the results to those of the `integral` procedure shown above.
;;
;; -------- (above from SICP)
;;

;; Compute cube of a number
(define (cube x) (* x x x))

;; We write a procedure that expresses the concept of summation itself
;; rather than only procedures that compute particular sums.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; The simpson integral procedure
(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (fun-iter k)
      (f (+ a (* k h))))
    (define (next m) (+ m 2))
    (* (/ h 3.0)
       (+ (f a)
          (f b)
          (* (sum fun-iter 1 next (- n 1)) 4)
          (* (sum fun-iter 2 next (- n 2)) 2)))))

;; The integral computation procedure above
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (main)
  (display "Load this file and use Simpson formula.\n")
  (display "We use `cube` function for test.\n")
  (display "[Original Integral] [dx = 0.01 ] ")
  (display (integral cube 0 1 0.01)) (newline)
  (display "[Simpson  Integral] [n  = 100  ] ")
  (display (simpson cube 0 1 100)) (newline)
  (display "[Original Integral] [dx = 0.001] ")
  (display (integral cube 0 1 0.001)) (newline)
  (display "[Simpson  Integral] [n  = 1000 ] ")
  (display (simpson cube 0 1 1000)) (newline))

(main)


