;; The solution of exercise 1.3
;; Define a procedure that takes three numbers as arguments and returns
;; the sum of the two larger numbers

;; Input three numbers from keyboard
;; Return the sum of the two larger numbers
(define (getBiggerSum a b c)
  (if (> a b)
      (if (> b c)
          (+ a b)
          (+ a c))
      (if (> a c)
          (+ a b)
          (+ c b))))

(define (main)
  (display "Input three numbers:")
  (newline)
  (let ((a (read)) (b (read)) (c (read)))
    (display (getBiggerSum a b c))
    (newline)))

(main)

