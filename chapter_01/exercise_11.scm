;; The solution of exercise 1.11
;; A function f is defined by the rule that f(n) = n if n < 3 and f(n) =
;; f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3) if n >= 3. Write a procedure
;; that computes f by means of a recursive process. Write a procedure that
;; computes f by means of an iterative process
;;

(define (recursive-f n)
  (if (<= n 3)
      n
      (+ (recursive-f (- n 1))
         (* 2 (recursive-f (- n 2)))
         (* 3 (recursive-f (- n 3))))))

(define (f-iter a b c count)
  (if (= count 1)
      c
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(define (iterative-f n)
  (f-iter 3 2 1 n))
  
(define (main)
  (display "Input n: ")
  (let ((n (read)))
    (display "[iterative process] f(") (display n) (display ") = ")
    (display (iterative-f n))
    (newline)
    (display "[recursive process] f(") (display n) (display ") = ")
    (display (recursive-f n)))
  (newline))

(main)


