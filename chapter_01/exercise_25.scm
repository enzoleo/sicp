;; The solution of exercise 1.25
;; Alyssa P. Hacker complains that we went to a lot of extra work in
;; writing `expmod`. After all, she says, since we already know how to
;; compute exponentials, we could have simply written
;;
;;     (define (expmod base exp m)
;;       (modulo (fast-expt base exp) m))
;;
;; Is she correct? Would this procedure serve as well for our fast prime
;; tester? Explain.
;;
;; -------- (above from SICP)
;;

;; Square a number
(define (square x) (* x x))

;; Fast exponential function
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((= 0 (modulo n 2)) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; To implement the Fermat test, we need a procedure that computes the
;; exponential of a number modulo another number
;; The new `expmod` procedure
(define (expmod base exp m)
  (modulo (fast-expt base exp) m))

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
;; The method by Alyssa is actually available. But note that for some big
;; numbers (such as 1,000,000), The efficiency is very very low. Because
;; it takes a lot of time to compute the result of expression a ^ n (n is
;; a huge number). Perhaps the result is so big that it causes stack
;; overflow.
;;
(define (main)
  (display "Find the two smallest primes larger than n.\n")
  (display "Here we set n = 10 ^ 4, 10 ^ 5, 10 ^ 6.\n")
  (compute-runtime 10000 2) (newline)
  (compute-runtime 100000 2) (newline)
  (compute-runtime 1000000 2) (newline))

(main)


