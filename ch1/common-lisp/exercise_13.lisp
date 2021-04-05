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
(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(defun fib (n)
  (fib-iter 1 0 n))

(defvar *phi* (/ (+ 1 (sqrt 5)) 2))
(defun phi-expt (n)
  (/ (expt *phi* n) (sqrt 5)))

;; List the comparison
(defun comparison-display (n)
  (do ((counter 0 (1+ counter)))
      ((> counter n))
    (format t "fibonacci(~d) ~c= ~d ~cphi-expt(~d) ~c= ~d ~%"
               counter #\tab (fib counter) #\tab
               counter #\tab (phi-expt counter))))

(defun main ()
  (format t "Compute Fibonacci number f(n).~%Input n: ")
  (let ((n (read)))
    (format t "[Fibonacci numbers] ~c[Exponential function - phi]~%"
            #\tab)
    (comparison-display n)))

(main)


