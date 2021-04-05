;; The solution of exercise 1.44
;; The idea of smoothing a function is an important concept in signal
;; processing. If f is a function and dx is some small number, then the
;; smoothed version of f is the function whose value at a point x is the
;; average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth
;; that takes as input a procedure that computes f and returns a procedure
;; that computes the smoothed f. It is sometimes valuable to repeatedly
;; smooth a function (that is, smooth the smoothed function, and so on) to
;; obtained the n-fold smoothed function. Show how to generate the `n-fold
;; smoothed function of any given function using `smooth` and `repeated`
;; from exercise 1.43.
;;
;; -------- (above from SICP)
;;

;; Define an infinitely small quantity
(define dx 0.00001)

;; Define composition procedure
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Necessary functions
(define (self-pow x) (expt x x))

;;
;; Repeated function. Actually this procedure is an iterative process. You
;; can compare the efficiency with the following procedure here:
;;
;;     (define (repeated f n)
;;       (define (iter counter result)
;;         (if (= counter n)
;;             result
;;             (iter (1+ counter) (f result))))
;;       (lambda (x)
;;         (iter 0 x)))
;;
;; But there exists an interesting thing:
;;
;;     (compose (repeated f (- n 1)) f) [ iterative process ]
;;     (compose f (repeated f (- n 1))) [ recursive process ]
;;
;; It is very easy to explain: the process type is determind by the
;; `compose` procedure.
;;

(define (repeated f n)
  (lambda (x)
    (cond ((= n 0) x)
          ((= n 1) (f x))
          (else ((compose (repeated f (- n 1)) f) x)))))

;; Smooth a function
(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (+ x dx))
          (f (- x dx)))
       3.0)))

;; Define a n fold smoothed function
(define (nf-smooth f n)
  ((repeated smooth n) f))

(define (main)
  (display "Load this file and use `nf-smooth` procedure.\n")
  (display "EXAMPLE: ((nf-smooth self-pow 5) 5)\n")
  (display "RESULT: ")
  (display ((nf-smooth self-pow 5) 5))
  (newline))

(main)


