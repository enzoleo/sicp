;; The solution of exercise 1.3
;; Define a procedure that takes three numbers as arguments and returns
;; the sum of the squares of the two larger numbers

;; Input three numbers from keyboard
;; Return the sum of the squares of the two larger numbers
(define (get-largest-sum-square a b c)
  (if (> a b)
      (if (> b c)
          (+ (* a a) (* b b))
          (+ (* a a) (* c c)))
      (if (> a c)
          (+ (* a a) (* b b))
          (+ (* c c) (* b b)))))

(define (main)
  (display "Input three numbers:")
  (newline)
  (let ((a (read)) (b (read)) (c (read)))
    (display "The sum of the squares of the two larger numbers: ")
    (display (get-largest-sum-square a b c))
    (newline)))

(main)

