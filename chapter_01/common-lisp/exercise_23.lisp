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
(defun next (n)
  (cond ((= n 2) 3)
        (t (+ n 2))))

;; Test a number is a prime or not
(defun prime? (n)
  ;; Inner nesting procedure
  ;; Find smallest divisor of n which is in [2, sqrt(n)] 
  (defun find-divisor (test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((= 0 (mod n test-divisor)) test-divisor)
          (t (find-divisor (next test-divisor)))))
  ;; Only positive integer is able to be prime
  ;; Particular case: 0 and 1 are not prime
  (if (<= n 1)
      nil
      (= n (find-divisor 2))))

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
(defun main ()
  (format t "Find The three smallest primes larger than n. ~%")
  (format t "Here we set n = 10 ^ 10, 10 ^ 11, 10 ^ 12. ~%")
  (compute-runtime 10000000000 3)
  (compute-runtime 100000000000 3)
  (compute-runtime 1000000000000 3))

(main)


