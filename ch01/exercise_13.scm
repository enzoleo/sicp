;; The solution of exercise 1.13
;; Prove that Fib(n) is the closest integer to phi ^ n / sqrt(5), where
;; phi = (1 + sqrt(5)) / 2. Hint: Let psi = (1 - sqrt(5)) / 2, then
;; use induction and the definition of the Fibonacci numbers (see section
;; 1.2.2) to prove that Fib(n) = (phi ^ n - psi ^ n) / sqrt(5)
;;
;; This exercise is a pure mathematical problem. The proof is not difficult
;; with the recursion formula of this sequence. Here we compute Fibonacci
;; numbers and do simple comparison.
;;

;; Compute Fibonacci numbers (linear steps)
;; There exists an algorithm for computing the Fibonacci numbers in a
;; logarithmic number of steps. (See Exercise 1.19)
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (fib n)
  (fib-iter 1 0 n))

(define phi (/ (+ 1 (sqrt 5)) 2))
(define (phi-expt n)
  (/ (expt phi n) (sqrt 5)))

;; List the comparison
(define (display-iter counter max-count)
  (if (<= counter max-count)
      (begin
        (display "fibonacci(") (display counter) (display ") \t")
        (display (fib counter))
        (display " \tphi-expt(") (display counter) (display ") \t")
        (display (phi-expt counter))
        (newline)
        (display-iter (+ counter 1) max-count))))

(define (comparison-display count)
  (display-iter 1 count))

(define (main)
  (display "Compute Fibonacci number f(n).\nInput n: ")
  (let ((n (read)))
    (display "[Fibonacci numbers] \t[Exponential function - phi]\n")
    (comparison-display n)))

(main)


