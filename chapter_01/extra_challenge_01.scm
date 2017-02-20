;; Page 46 - Example: Counting change
;; Consider the following problem: How many different ways can we make
;; change of $1.00, given half-dollars, quarters, dimes, nickels, and
;; pennies? More generally, can we write a procedure to compute the number
;; of ways to change any given amount of money?
;;
;; The SICP book gives us a method using recursion:
;; The nunmber of ways to change amount a using n kinds of coins equals
;;
;;     [1] the number of ways to change amount a using all but the first
;;         kind of coin, plus
;;
;;     [2] the number of ways to change amount a - d using all n kinds of
;;         coins, where d is the denomination of the first kind of coin
;;
;; We recursively reduce the problem of changing a given amount to the
;; problem of changing smaller amounts using fewer kinds of coins.
;; ------- above from SICP
;;
;; Here we give an example to design a better algorithm for computing the
;; correct result given by recursive algorithm.
;;

;; Describe recursive algorithm first
(define (count-change-rec amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;
;; From the description of recursive algorithm, we can deduce the recursion
;; formula, which may help us think up of a better algorithm.
;;
;;     count-change(n) = cc(n, 5)
;;
;;                 | 1                               , x = 0
;;     cc(x, y) = <  0                               , x < 0 or y = 0
;;                 | cc(x, y - 1) + cc(x - koc(y), y), else
;;
;;     ----------------------------------------
;;     |   y    |  1  |  2  |  3  |  4  |  5  |
;;     ----------------------------------------
;;     | koc(y) |  1  |  5  |  10 |  25 |  50 |
;;     ----------------------------------------
;;
;; We know that the range of variable y is {0, 1, 2, 3, 4, 5}. So we can
;; consider cc(x, 0) to cc(x, 5) one by one.
;;
;;     cc(x, 0) = 0
;;     cc(x, 1) = cc(x, 0) + cc(x - 1, 1)
;;     cc(x, 2) = cc(x, 1) + cc(x - 5, 2)
;;     cc(x, 3) = cc(x, 2) + cc(x - 10, 3)
;;     cc(x, 4) = cc(x, 3) + cc(x - 25, 4)
;;     cc(x, 5) = cc(x, 4) + cc(x - 50, 5)
;;
;; Thus we have:
;;
;;     cc(x, 1) = cc(x - 1, 1) = ... = cc(0, 1) = 1
;;     cc(x, 2) = cc(x - 5, 2) + 1 = ... = 1 + [x / 5]
;;     cc(x, 3) = cc(x - 10, 3) + [x / 5] + 1 = ...
;;
;;                 | ([x / 10] + 1) ^ 2             , x % 10 < 5 
;;              = <
;;                 | ([x / 10] + 1) * ([x / 10] + 2), x % 10 >= 5
;;
;; where the function [x / k] = x - x % k, is Gauss Function.
;;
;; Here we list the explicit formula to compute cc(x, i) (i in {1, 2, 3}).
;; But it is quite difficult to deduce the similar formulas to compute
;; cc(x, 4) and cc(x, 5). So we have to compute cc(x, 5) according to the
;; recursion formulas listed above. We rewrite the recursion formula:
;;
;;                [x / koc(y)]
;;     cc(x, y) =    sigma     cc(x - k * koc(y), y - 1)   (y >= 2)
;;                   k = 0
;;
;;     cc(x, 1) = 1, for all x >= 0.
;;
;; This formula is much more intuitive to compute the results, because we
;; can compute each cc(x, y) with some cc(x', y - 1) values, which looks
;; like descending dimension method in some sense. So we can extract an
;; interesting algorithm to compute cc(x, 5).
;;
;;     [1] First make a vector with length (x + 1): 1, 1, 1, ... , 1, 1
;;         This vector is equivalent to the sequence:
;;             cc(0, 1), cc(1, 1), ... , cc(x, 1).
;;         Let y = 1.
;;
;;     [2] Then we compute cc(x, 2) using the recursion formula.
;;         for vec-index in koc(y), koc(y) + 1, ... , amount
;;                                 [x / koc(y)]
;;             cc(vec-index, y + 1) = sum    cc(vec-index - k * koc(y), y)
;;                                   k = 0
;;         Let y = y + 1.
;;
;;     [3] If y <= 4 return the step [2];
;;         else stop, return the last element in the vector.
;; 
;; Now we realize this algorithm with scheme.
;;

;; This cc-compute procedure computes cc(x, y) vectors (x in {0, 1, ... ,
;; amount} with cc(x, y - 1) vectors, interval (equivalent to koc) needed.
(define (cc-compute vec-buf interval)
  (define vec-len (vector-length vec-buf))
  (define vec-index 0)

  ;; Another function (nesting) definition (recursion)
  ;; This inner function adds elements in vector to the elements with
  ;; distance equaling to `interval` from them.
  (define (add-element index)
    (if (and (<= 0 index) (< (+ index interval) vec-len))
        (begin
          (let ((tmp (vector-ref vec-buf (+ index interval))))
            (vector-set! vec-buf (+ index interval)
                         (+ (vector-ref vec-buf index)
                            tmp)))
          (add-element (+ index 1)))))
  (add-element vec-index))

;; This new procedure computes the number of ways to do count change
(define (count-change-itr amount)
  (define cc-buf (make-vector (+ amount 1) 1))
  (define (cc-compute-itr kinds-of-coins)
    (if (< kinds-of-coins 5)
        (begin
          (cc-compute cc-buf (first-denomination (+ kinds-of-coins 1)))
          (cc-compute-itr (+ kinds-of-coins 1)))
        (vector-ref cc-buf amount)))
  (cc-compute-itr 1))

;; Input some big value such as 500, 600, 700 ... and then simply test the
;; performance of two algorithms. Apparently, the new method performs much
;; better. Actually the new method has linear complexity while the other
;; has exponential complexity.

(define (main)
  (display "How many cents will you change? ")
  (let ((amount (read)))
    (display "The result using iteration: ")
    (display (count-change-itr amount))
    (newline)
    (display "The result using recursion: ")
    (display (count-change-rec amount)))
  (newline))

(main)


