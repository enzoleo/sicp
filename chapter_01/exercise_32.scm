;; The solution of exercise 1.32
;; [a] Show that sum and product (exercise 1.31) are both special cases of
;;     a still more general notion called accumulate that combines a
;;     collection of terms, using some general accumulation function:
;;     (accumulate combiner null-value term a next b) Accumulate takes as
;;     arguments the same term and range specifications as sum and product,
;;     together with a combiner procedure (of two arguments) that specifies
;;     how the current term is to be combined with the accumulation of the
;;     preceding terms and a null-value that specifies what base value to
;;     use when the terms run out. Write accumulate and show how sum and
;;     product can both be defined as simple calls to accumulate.
;;
;; [b] If your accumulate procedure generates a recursive process, write
;;     one that generates an iterative process. If it generates an
;;     iterative process, write one that generates a recursive process.
;;
;; -------- (above from SICP)
;;

;; The square procedure
(define (square x) (* x x))

;; The abstract accumulate procedure (iterative and recursive process)
(define (accumulate combiner null-value term a next b)
  (define (iter m result)
    (if (> m b)
        result
        (iter (next m) (combiner result (term m)))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value
                                term
                                (next a) next b))))

;; New abstract sum procedure (iterative and recursive process)
;; Use accumulate procedure (combiner = "+" procedure)
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (sum-rec term a next b)
  (accumulate-rec + 0 term a next b))

;; New abstract product procedure (iterative and recursive process)
;; Use accumulate procedure (combiner = "*" procedure)
(define (product term a next b)
  (accumulate * 1 term a next b))
(define (product-rec term a next b)
  (accumulate-rec * 1 term a next b))

;; Wallis formula is used to test `product`
(define (wallis-pi n)
  (define (term m)
    (square (/ (* m 2.0) (- (* m 2) 1))))
  (/ (* 8.0
        (product term
                 2
                 (lambda (x) (+ x 1))
                 n))
     (* n 2)))

;; Leibniz formula is used to test `sum`
(define (leibniz-pi n)
  (* 8.0
     (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
          1
          (lambda (x) (+ x 4))
          n)))

(define (main)
  (display "Load this file and use `accumulate` procedure.\n")
  (display "We use `accumulate` to define `sum` and `product`.\n")
  (display "Use `sum`, `sum-rec`, `product` and `product-rec`.\n")
  (display "Test product: [Wallis formula ]: PI = ")
  (display (wallis-pi 10000000)) (newline)
  (display "Test sum    : [Leibniz formula]: PI = ")
  (display (leibniz-pi 10000000)) (newline)
  (display "(We compute 10 ^ 7 terms in the two tests above)") 
  (newline))

(main)


