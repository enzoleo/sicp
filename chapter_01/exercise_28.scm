;; The solution of exercise 1.28
;; One variant of the Fermat test that cannot be fooled is called the
;; Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an
;; alternate form of Fermat's Little Theorem, which states that if n is a
;; prime number and a is any positive integer less than n, then a raised to
;; the (n - 1)st power is congruent to 1 modulo n. To test the primality of
;; a number n by the Miller-Rabin test, we pick a random number a < n and
;; raise a to the (n - 1)st power modulo n using the `expmod` procedure.
;; However, whenever we perform the squaring step in `expmod`, we check to
;; see if we have discovered a "nontrivial square root of 1 modulo n", that
;; is, a number not equal to 1 or n - 1 whose square is equal to 1 modulo
;; n. (This is why Miller-Rabin test cannot be fooled.)
;;
;; Modify the `expmod` procedure to signal if it discovers a nontrivial
;; square root of 1, and use this to implement the Miller-Rabin test with a
;; procedure analogous to `fermat-test`. Check your procedure by testing
;; various known primes and non-primes.
;;
;; HInt: One convenient way to make `expmod` signal is to have it return 0.
;;
;; -------- (above from SICP)
;;

;; Square a number
(define (square x) (* x x))

;; Check whether number @m has nontrivial square root of 1
(define (nontrivial-sqrt? base m)
  (and (not (= base 1))
       (not (= base (- m 1)))
       (= 1 (modulo (square base) m))))

;; To implement the Fermat test, we need a procedure that computes the
;; exponential of a number modulo another number. This is a new `expmod`
;; procedure including nontrivial square root detection.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial-sqrt? base m) 0)
        ((= 0 (modulo exp 2))
         (modulo (square (expmod base (/ exp 2) m))
                 m))
        (else
         (modulo (* base (expmod base (- exp 1) m))
                 m))))

;; Choose a random number between 1 and n - 1 using the procedure `random`,
;; which we assume is included as a primitive in Scheme. This is Miller-
;; Rabin test instead of Fermat test.
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

;; The following procedure runs the test a given number of times, as
;; specified by a parameter. Its value is true if the test succeeds every
;; time, and false otherwise.
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

;;
;; Test a number is a prime or not. Here we do m-r-test @times times, where
;; parameter @times = 20.
;;
;; Of course you can change the miller-rabin-test times. For example, you
;; can set @times = (ceiling (/ n 2)) to ensure enough probability of
;; success.
;;
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

;; This procedure search all odd primes between lower and upper and display
;; all of them onto the screen. We use the original procedure of exercise
;; 1.22 here.
(define (search-for-primes lower upper)
  (define (test-odd-primes counter max-count)
    (if (<= counter max-count)
        (begin
          (if (prime? counter)
              (begin
                (display "prime: ")
                (display counter)
                (newline)))
          (test-odd-primes (+ counter 2) max-count))))
  ;; Pass odd parameters to test-odd procedure
  (test-odd-primes (if (= 0 (modulo lower 2)) (+ lower 1) lower)
                   (if (= 0 (modulo upper 2)) (- upper 1) upper)))

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
;; Test 561 in both Fermat test and Miller-Rabin test.
;; guile:
;; [1.24] > (prime? 561)
;;        $1 = #t
;; [1.28] > (prime? 561)
;;        $1 = #f
;;

(define (main)
  (display "Load this file and use Miller-Rain test.\n")
  (display "Simply using (prime? n) is available.\n"))

(main)


