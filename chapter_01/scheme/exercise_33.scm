;; The solution of exercise 1.33
;; You can obtain an even more general version of `accumulate` (exercise
;; 1.32) by introducing the notion of a filter on the terms to be combined.
;; That is, combine only those terms derived from values in the range that
;; satisfy a specified condition. The resulting `filtered-accumulate`
;; abstraction takes the same arguments as accumulate, together with an
;; additional predicate of one argument that specifies the filter. Write
;; `filtered-accumulate` as a procedure. Show how to express the following
;; using `filtered-accumulate`:
;;
;; [a] the sum of the squares of the prime numbers in the interval a to b
;;     (assuming that you have a `prime?` predicate already written)
;;
;; [b] the product of all the positive integers less than n that are
;;     relatively prime to n (i.e., all positive integers i < n such that
;;     GCD(i, n) = 1).
;;
;; -------- (above from SICP)
;;

;; The square procedure
(define (square x) (* x x))

;; The abstract accumulate procedure (iterative process)
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter m result)
    (if (> m b)
        result
        (if (filter m)
            (iter (next m) (combiner result (term m)))
            (iter (next m) result))))
  (iter a null-value))

;; Test a number is a prime or not
(define (prime? n)
  ;; Inner nesting procedure
  ;; Find smallest divisor of n which is in [2, sqrt(n)] 
  (define (find-divisor test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((= 0 (modulo n test-divisor)) test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  ;; Only positive integer is able to be prime
  ;; Particular case: 0 and 1 are not prime
  (if (or (<= n 1) (not (integer? n)))
      #f
      (= n (find-divisor 2))))

;; The procedure computes the greatest common divisors of two integers.
;; We use the procedure on the Page 51.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; New abstract filtered sum procedure (iterative process)
;; Use `filtered-accumulate` procedure (combiner = "+" procedure)
(define (filtered-sum filter term a next b)
  (filtered-accumulate filter + 0 term a next b))

;; New abstract filtered product procedure (iterative process)
;; Use `filtered-accumulate` procedure (combiner = "*" procedure)
(define (filtered-product filter term a next b)
  (filtered-accumulate filter * 1 term a next b))

;; Compute the sum of the squares of the prime numbers in the interval a
;; to b (we already have `prime?` procedure)
(define (square-prime-sum a b)
  (filtered-sum prime?
                square
                a
                (lambda (x) (+ x 1))
                b))

;; Compute the the product of all the positive integers less than n that
;; are relatively prime to n.
(define (coprime-product n)
  (filtered-product (lambda (x) (= 1 (gcd n x)))
                    (lambda (x) x)
                    1
                    (lambda (x) (+ x 1))
                    n))

(define (main)
  (display "Load this file and use `filtered-accumulate` procedure.\n")
  (display "We only define `filtered-sum` and `filtered-product` as ")
  (display "iterative process.\n")
  (display "Test `square-prime-sum` and `coprime-product` procedure:\n")
  (display "(square-prime-sum 106 300) = ")
  (display (square-prime-sum 106 300)) (newline)
  (display "(coprime-product 30) = ")
  (display (coprime-product 30))
  (newline))

(main)


