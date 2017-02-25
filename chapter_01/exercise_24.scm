;; The solution of exercise 1.24
;; Modify the `timed-prime-test` procedure of exercise 1.22 to use
;; `fast-prime?` (the Fermat method), and test each of the 12 primes you
;; found in that exercise. Since the Fermat test has O(log n) growth, how
;; would you expect the time to test primes near 1,000,000 to compare with
;; the time needed to test primes near 1000? Do your data bear this out?
;; Can you explain any discrepancy you find?
;;
;; -------- (above from SICP)
;;

;; Square a number
(define (square x) (* x x))

;; To implement the Fermat test, we need a procedure that computes the
;; exponential of a number modulo another number
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((= 0 (modulo exp 2))
         (modulo (square (expmod base (/ exp 2) m))
                 m))
        (else
         (modulo (* base (expmod base (- exp 1) m))
                 m))))

;; Choose a random number between 1 and n - 1 using the procedure `random`,
;; which we assume is included as a primitive in Scheme.
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; The following procedure runs the test a given number of times, as
;; specified by a parameter. Its value is true if the test succeeds every
;; time, and false otherwise.
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; Test a number is a prime or not. Here we do fermat-test 20 times for
;; each number n. You can modify the number as you like.
(define (prime? n)
  (fast-prime? n 20))

(define (next-prime n)
  (define (test-prime counter)
    (if (prime? counter)
        counter
        (test-prime (+ counter 1))))
  (test-prime (+ n 1)))

(define (search-for-next-primes n m)
  (define (search-count init-num counter)
    (if (< counter m)
        (begin
          (let ((new-prime (next-prime init-num)))
          (display "prime[") (display counter) (display "] ")
          (display new-prime)
          (newline)
          (search-count new-prime (+ counter 1))))))
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

;; Compute the runtime of @search-for-next-primes
(define (compute-runtime n m)
  (define (clause-runtime start-time)
    (search-for-next-primes n m)
    (display "runtime: ")
    (display (- (runtime) start-time)))
  (clause-runtime (runtime))
  (newline))

;;
;; Actually the expectation is not confirmed. Because the runtime is
;; determined by many factors: the occupation of system resources, the
;; optimization of interpreter of compiler, etc. We can not predict the
;; runtime of one procedure precisely.
;;
(define (main)
  (display "Find The three smallest primes larger than n.\n")
  (display "Here we set n = 10 ^ 10, 10 ^ 11, 10 ^ 12.\n")
  (compute-runtime 10000000000 3) (newline)
  (compute-runtime 100000000000 3) (newline)
  (compute-runtime 1000000000000 3) (newline))

(main)


