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
(defun square (x) (* x x))

;; Check whether number @m has nontrivial square root of 1
(defun nontrivial-sqrt? (base m)
  (and (not (= base 1))
       (not (= base (- m 1)))
       (= 1 (mod (square base) m))))

;; To implement the Fermat test, we need a procedure that computes the
;; exponential of a number modulo another number. This is a new `expmod`
;; procedure including nontrivial square root detection.
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial-sqrt? base m) 0)
        ((= 0 (mod exp 2))
         (mod (square (expmod base (/ exp 2) m))
              m))
        (t (mod (* base (expmod base (- exp 1) m))
                m))))

;; Choose a random number between 1 and n - 1 using the procedure `random`,
;; which we assume is included as a primitive in Scheme. This is Miller-
;; Rabin test instead of Fermat test.
(defun miller-rabin-test (n)
  (defun try-it (a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

;; The following procedure runs the test a given number of times, as
;; specified by a parameter. Its value is true if the test succeeds every
;; time, and false otherwise.
(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (t nil)))

;;
;; Test a number is a prime or not. Here we do m-r-test @times times, where
;; parameter @times = 20.
;;
;; Of course you can change the miller-rabin-test times. For example, you
;; can set @times = (ceiling (/ n 2)) to ensure enough probability of
;; success.
;;
(defun prime? (n)
  (fast-prime? n 20))

;; Find the smallest prime that is bigger than n
(defun next-prime (n)
  (let ((init-counter (if (= 0 (mod n 2)) (+ 1 n) (+ 2 n))))
    (if (< n 2)
        2
        (do ((counter init-counter (+ 2 counter)))
            (nil)
          (if (prime? counter)
              (return counter))))))

;; Find the smallest m primes that are bigger than n
(defun search-for-next-primes (n m)
  (defun search-count (init-num counter)
    (if (< counter m)
        (progn
          (let ((new-prime (next-prime init-num)))
          (format t "prime[~d] ~d ~%" counter new-prime)
          (search-count new-prime (+ counter 1))))))
  (search-count n 0))

;; This procedure search all odd primes between lower and upper.
;; all of them onto the screen. We use the original procedure of exercise
;; 1.22 here.
(defun search-for-primes (lower upper)
  (defun test-odd-primes (init-count max-count)
    (do ((counter init-count (+ 2 counter)))
        ((> counter max-count))
      (if (prime? counter)
          (format t "prime: ~d ~%" counter))))
  ;; Pass odd parameters to test-odd procedure
  (test-odd-primes (if (= 0 (mod lower 2)) (+ lower 1) lower)
                   (if (= 0 (mod upper 2)) (- upper 1) upper)))

;;
;; Not every implementation has the procedure `runtime`. You can
;; use alternatives for different interpreter.
;;
;; [SBCL]
;;     (defun runtime () (get-internal-real-time))
;;
(defun runtime() (get-internal-real-time))

;; Compute the runtime of @search-for-next-primes
(defun compute-runtime (n m)
  (defun clause-runtime (start-time)
    (search-for-next-primes n m)
    (format t "runtime: ~a ~%" (- (runtime) start-time)))
  (clause-runtime (runtime)))

;;
;; Test 561 in both Fermat test and Miller-Rabin test.
;; SBCL:
;; [1.24] * (prime? 561)
;;        T
;; [1.28] * (prime? 561)
;;        NIL
;;

(defun main ()
  (format t "Load this file and use Miller-Rain test. ~%")
  (format t "Simply using (prime? n) is available. ~%"))

(main)


