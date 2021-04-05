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
(defun square (x) (* x x))

;; Fast exponential function
(defun fast-expt (b n)
  (cond ((= n 0) 1)
        ((= 0 (mod n 2)) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

;; To implement the Fermat test, we need a procedure that computes the
;; exponential of a number modulo another number
;; The new `expmod` procedure
(defun expmod (base exp m)
  (mod (fast-expt base exp) m))

;; Choose a random number between 1 and n - 1 using the procedure `random`,
;; which we assume is included as a primitive in Scheme.
(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; The following procedure runs the test a given number of times, as
;; specified by a parameter. Its value is true if the test succeeds every
;; time, and false otherwise.
(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t nil)))

;; Test a number is a prime or not. Here we do fermat-test 20 times for
;; each number n. You can modify the number as you like.
(defun prime? (n)
  (fast-prime? n 20))

;; Find the smallest prime that is bigger than n
(defun next-prime (n)
  ;; The next odd number larger than n
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

;; Load this file in SBCL and run this `main` function to do some tests
;; Using clisp is probably slow and may cause `Program stack overflow`.
;;
;; The method by Alyssa is actually available. But note that for some big
;; numbers (such as 1,000,000), The efficiency is very very low. Because
;; it takes a lot of time to compute the result of expression a ^ n (n is
;; a huge number). Perhaps the result is so big that it causes stack
;; overflow.
;;

(defun main ()
  (format t "Find The three smallest primes larger than n. ~%")
  (format t "Here we set n = 10 ^ 4, 10 ^ 5, 10 ^ 6. ~%")
  (compute-runtime 10000 3)
  (compute-runtime 100000 3)
  (compute-runtime 1000000 3))

(main)


