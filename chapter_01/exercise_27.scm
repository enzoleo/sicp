;; The solution of exercise 1.27
;;
;; -------- (above from SICP)
;;

;; The square procedure
(define (square x) (* x x))

;; The modified `expmod` procedure
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((= 0 (modulo exp 2))
         (modulo (square (expmod base (/ exp 2) m))
                 m))
        (else
         (modulo (* base (expmod base (- exp 1) m))
                 m))))

;; This procedure points out whether the number is a carmichael number
;; If number @n is a prime, it is not a carmichael number. But if @n is not
;; a prime, use `carmichael-iter` procedure to check all numbers smaller
;; than @n whether they satisfy the expression: n | a ^ n - a. If all of
;; them satisfy this expression, the number @n is a carmichael number.
(define (carmichael? n)
  (if (prime? n)
      #f
      (carmichael-iter n 1)))

;; Iterative process to check carmichael number
(define (carmichael-iter n counter)
  (cond ((= counter n) #t)
        ((= counter (expmod counter n n))
         (carmichael-iter n (+ counter 1)))
        (else #f)))

;; Test a number is a prime or not
(define (prime? n)
  (define (find-divisor test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((= 0 (modulo n test-divisor)) test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  (if (or (<= n 1) (not (integer? n)))
      #f
      (= n (find-divisor 2))))

;; Search the smallest carmichael number larger than n
(define (next-carmichael n)
  (define (test-carmichael counter)
    (if (carmichael? counter)
        counter
        (test-carmichael (+ counter 1))))
  (test-carmichael (+ n 1)))

;; Search the m smallest carmichael numbers larger than n
(define (search-carmichael n m)
  (define (search-count init-num counter)
    (if (< counter m)
        (begin
          (let ((new-carmichael (next-carmichael init-num)))
          (display "carmichael[") (display counter) (display "]\t")
          (display new-carmichael)
          (newline)
          (search-count new-carmichael (+ counter 1))))))
  (search-count n 0))

;;
;; Actually not every implementation has the procedure `runtime`. You can
;; use alternatives for different interpreter:
;;
;; [1] guile:
;;     (define (runtime) (tms:clock (times)))
;;
;; [2] racket
;;     (define (runtime) (current-milliseconds))
;;
;; In mit-scheme, use (runtime) directly.
;;
(define (runtime) (tms:clock (times)))

;; Compute the runtime of @search-carmichael
;; Compute the runtime to find the first m carmichael numbers
(define (compute-runtime m)
  (define (clause-runtime start-time)
    (search-carmichael 1 m)
    (display "runtime: ")
    (display (- (runtime) start-time)))
  (clause-runtime (runtime))
  (newline))

;; Here we compute 30 smallest carmichael numbers:
;;
;; Find 30 smallest Carmichael numbers:
;; carmichael[0]	561
;; carmichael[1]	1105
;; carmichael[2]	1729
;; carmichael[3]	2465
;; carmichael[4]	2821
;; carmichael[5]	6601
;; carmichael[6]	8911
;; carmichael[7]	10585
;; carmichael[8]	15841
;; carmichael[9]	29341
;; carmichael[10]	41041
;; carmichael[11]	46657
;; carmichael[12]	52633
;; carmichael[13]	62745
;; carmichael[14]	63973
;; carmichael[15]	75361
;; carmichael[16]	101101
;; carmichael[17]	115921
;; carmichael[18]	126217
;; carmichael[19]	162401
;; carmichael[20]	172081
;; carmichael[21]	188461
;; carmichael[22]	252601
;; carmichael[23]	278545
;; carmichael[24]	294409
;; carmichael[25]	314821
;; carmichael[26]	334153
;; carmichael[27]	340561
;; carmichael[28]	399001
;; carmichael[29]	410041
;;

(define (main)
  (display "Find 30 smallest Carmichael numbers:\n")
  (compute-runtime 30) (newline))

(main)


