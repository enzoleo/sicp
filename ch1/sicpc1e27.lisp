;; The solution of exercise 1.27
;; Demonstrate that the Carmichael numbers listed in footnote 47 really do
;; fool the Fermat test. That is, write a procedure that takes an integer
;; n and tests whether a ^ n is congruent to a modulo n for every a < n,
;; and try your procedure on the given Carmichael numbers.
;;
;; -------- (above from SICP)
;;

;; The square procedure
(defun square (x) (* x x))

;; To implement the Fermat test, we need a procedure that computes the
;; exponential of a number modulo another number
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((= 0 (mod exp 2))
         (mod (square (expmod base (/ exp 2) m))
                 m))
        (t (mod (* base (expmod base (- exp 1) m))
                 m))))

;; Test a number is a prime or not
(defun prime? (n)
  ;; Inner nesting procedure
  ;; Find smallest divisor of n which is in [2, sqrt(n)] 
  (defun find-divisor (test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((= 0 (mod n test-divisor)) test-divisor)
          (t (find-divisor (+ test-divisor 1)))))
  ;; Only positive integer is able to be prime
  ;; Particular case: 0 and 1 are not prime
  (if (<= n 1)
      nil
      (= n (find-divisor 2))))

;;
;; This procedure `carmichael` points out whether the given number is a
;; carmichael number. If number @n is a prime, it is not a carmichael
;; number. But if @n is not a prime, use `carmichael-iter` procedure to
;; check all numbers smaller than @n whether they satisfy the expression:
;; n | a ^ n - a. If all of them satisfy this expression, the number @n
;; is a carmichael number.
;;
;; Iterative process to check carmichael number
(defun carmichael-iter (n counter)
  (cond ((= counter n) t)
        ((= counter (expmod counter n n))
         (carmichael-iter n (+ counter 1)))
        (t nil)))

;; Honestly to say, simply using recursion like this (iterative process)
;; is sometimes better than other advanced loops. Using advanced loops may
;; need more local parameters to describe and sometimes the efficiency is
;; probably low because of our careless negligence on process design.
;;
;;    (defun carmichael? (n)
;;      (if (prime? n)
;;          nil
;;          (carmichael-iter n 1)))
;;
;; Here we still use do-loop. The procedure above is also available. The
;; runtime in SBCL is quite close.
;;
(defun carmichael? (n)
  (if (prime? n)
      nil
      (let ((carmichael-flag t))
        (do ((counter 1 (1+ counter)))
            ((>= counter n))
          (if (not (= counter (expmod counter n n)))
              (progn
                (setf carmichael-flag nil)
                (return))))
        carmichael-flag)))

;; Search the smallest carmichael number larger than n
(defun next-carmichael (n)
  (defun test-carmichael (counter)
    (if (carmichael? counter)
        counter
        (test-carmichael (+ counter 1))))
  (test-carmichael (+ n 1)))

;; Search the m smallest carmichael numbers larger than n
(defun search-carmichael (n m)
  (defun search-count (init-num counter)
    (if (< counter m)
        (progn
          (let ((new-carmichael (next-carmichael init-num)))
            (format t "carmichael[~d]~c ~d ~%"
                    counter #\tab new-carmichael)
          (search-count new-carmichael (+ counter 1))))))
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
(defun compute-runtime (m)
  (defun clause-runtime (start-time)
    (search-carmichael 1 m)
    (format t "runtime: ~a ~%" (- (runtime) start-time)))
  (clause-runtime (runtime)))


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

(defun main ()
  (format t "Find 30 smallest Carmichael numbers: ~%")
  (compute-runtime 30))

(main)


