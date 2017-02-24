;; The solution of exercise 1.23
;; The `smallest-divisor` procedure shown at the start of this section does
;; lots of needless testing: After it checks to see if the number is
;; divisible by 2 there is no point in checking to see if it is divisible
;; by any larger even numbers. This suggests that the values used for
;; `test-divisor` should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7,
;; 9,... . To implement this change, define a procedure `next` that returns
;; 3 if its input is equal to 2 and otherwise returns its input plus 2.
;; Modify the `smallest-divisor` procedure to use (next test-divisor)
;; instead of (+ test-divisor 1). With `timed-prime-test` incorporating
;; this modified version of `smallest-divisor`, run the test for each of
;; the 12 primes found in exercise 1.22. Since this modification halves the
;; number of test steps, you should expect it to run about twice as fast.
;; Is this expectation confirmed? If not, what is the observed ratio of the
;; speeds of the two algorithms, and how do you explain the fact that it is
;; different from 2?
;;
;; -------- (above from SICP)
;;

;; The `next` procedure
(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))
  
;; Test a number is a prime or not
(define (prime? n)
  (define (find-divisor test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((= 0 (modulo n test-divisor)) test-divisor)
          (else (find-divisor (next test-divisor)))))
  (if (or (<= n 1) (not (integer? n)))
      #f
      (= n (find-divisor 2))))

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

(define (main)
  (display "Find The three smallest primes larger than n.\n")
  (display "Here we set n = 10 ^ 10, 10 ^ 11, 10 ^ 12.\n")
  (compute-runtime 10000000000 3) (newline)
  (compute-runtime 100000000000 3) (newline)
  (compute-runtime 1000000000000 3) (newline))

(main)


