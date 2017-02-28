;; The solution of exercise 1.30
;; The sum procedure above generates a linear recursion. The procedure
;; can be rewritten so that the sum is performed iteratively. Show how
;; to do this process.
;;
;; -------- (above from SICP)
;;

;; Compute cube of a number
(define (cube x) (* x x x))

;; New abstrack summation procedure (iterative process)
(define (sum term a next b)
  (define (iter m result)
    (if (> m b)
        result
        (iter (next m) (+ result (term m)))))
  (iter a 0))

;; The simpson integral procedure
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (fun-iter k)
    (f (+ a (* k h))))
  (define (next m) (+ m 2))
  (* (/ h 3.0)
     (+ (f a)
        (f b)
        (* (sum fun-iter 1 next (- n 1)) 4)
        (* (sum fun-iter 2 next (- n 2)) 2))))

;; The integral computation procedure above
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (main)
  (display "Load this file and use Simpson formula. (iteratively)\n")
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


