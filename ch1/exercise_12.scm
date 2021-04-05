;; The solution of exercise 1.12
;; The following pattern of numbers is called Pascal's triangle.
;;
;;                          1
;;                        1   1
;;                      1   2   1
;;                    1   3   3   1
;;                  1   4   6   4   1
;;
;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it. Write a
;; procedure that computes elements of Pascal's triangle by means of a
;; recursive process.
;;

;; Compute combinarotial number by means of a recursive process
(define (recursive-pascal m n)
  (cond ((or (< m 0) (< n 0) (< m n))
         (display "Wrong number in PASCAL triangle.\n"))
        ((or (= n 0) (= n m)) 1)
        (else (+ (recursive-pascal (- m 1) (- n 1))
                 (recursive-pascal (- m 1) n)))))

;; The procedure computes factorial numbers by means of an iterative
;; process, with linear complexity.
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial n)  
  (fact-iter 1 1 n))

;;
;; We can use the mathematical definition of combinatorial numbers to
;; simply computes them:
;;
;;                    m!                  factorial(m)
;;     C(m, n) = ------------- = -------------------------------
;;                n! (m - n)!    factorial(n) * factorial(m - n)
;;
;;                m * (m - 1) * ... * (m - n + 1)
;;             = ---------------------------------
;;                          factorial(n)
;;
;; Thus we can write a new procedure in scheme and it is very simple.
;; Notice that:
;;
;;     fact-iter(1, a, b) = a * (a + 1) * ... * (b - 1) * b
;;
;; We have: C(m, n) = fact-iter(1, m - n + 1, m) / fact-iter(1, 1, n)
;; This formula only does (2 * n - 1) times of multiplication / division.
;;

;; Computes the combinatorial numbers
(define (combinatorial-num m n)
  (if (> (* 2 n) m)
      (/ (fact-iter 1 (+ n 1) m)
         (fact-iter 1 1 (- m n)))
      (/ (fact-iter 1 (- m (- n 1)) m)
         (fact-iter 1 1 n))))

;;
;; There also exists another algorithm, according to the formula given by
;; the Pascal's triangle:
;;
;;     C(m, n) = C(m - 1, n - 1) + C(m - 1, n)    (m, n >= 1)
;;
;; We make a vector v with init value #(1, 0, 0, ... , 0) with length =
;; n + 1. and then make a new vector v':
;;
;;     v'(n) = v(n) + v(n - 1)   (1 <= n <= count)
;;
;; where the variable `count` means the number of non-zero elements in the
;; vector v. After this operation, we get v' = #(1, 1, 0, ... , 0) with
;; length = n + 1. Continue doing such an operation on v', we get v'' =
;; #(1, 2, 1, ... , 0) and v''' = #(1, 3, 3, 1, ... , 0). We can get all
;; the elements on m th row in Pascal's triangle, using this algorithm.
;; Besides, the algorithm behaves well on complexity analysis, for we only
;; do O(m ^ 2) times of addition to compute all numbers C(m, k).
;;
;; Now we realize this algorithm in scheme.
;;

(define (pascal-compute vec-buf new-vec-buf max-count)
  (define vec-index 0)
  (define (add-element index)
    (if (and (<= 0 index) (< index max-count))
        (begin
          (vector-set! new-vec-buf (+ index 1)
                       (+ (vector-ref vec-buf index)
                          (vector-ref vec-buf (+ index 1))))
          (add-element (+ index 1)))))
  (add-element vec-index))

;; This new procedure computes the number of ways to do count change
(define (pascal m n)
  (define pascal-buf (make-vector (+ m 1) 0))
  (define new-pascal-buf (make-vector (+ m 1) 0))
  (define (pascal-compute-itr vec-buf new-vec-buf counter)
    (if (<= counter m)
        (begin
          (pascal-compute vec-buf new-vec-buf counter)
          (pascal-compute-itr new-vec-buf vec-buf (+ counter 1)))
        (vector-ref vec-buf n)))
  (vector-set! pascal-buf 0 1)
  (vector-set! new-pascal-buf 0 1)
  (pascal-compute-itr pascal-buf new-pascal-buf 1))

(define (main)
  (display "Compute pascal number C(m, n).\nInput m and n: ")
  (let ((m (read)) (n (read)))
    (display "[combinatory formula] \tThe pascal number: ")
    (display (combinatorial-num m n))
    (newline)
    (display "[iterative process] \tThe pascal number: ")
    (display (pascal m n))
    (newline)
    (display "[recursive process] \tThe pascal number: ")
    (display (recursive-pascal m n)))
  (newline))

(main)


