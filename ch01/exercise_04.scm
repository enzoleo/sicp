;; The solution of exercise 04
;; Observe that our model of evaluation allows for combinations whose
;; operators are compound expressions. Use this observation to describe
;; the behavior of the following procedure.

(define (plus-abs a b)
  ((if (> b 0) + -) a b))

;; Note how the function works.
;; The expression above is equivalent to the math equation: a + abs(b)
;; (if (> b 0) + -) returns function `+` if b > 0 is true.
;; Notice that `+` and `-` may be operators in some other programming
;; language, but in Scheme they are functions instead.

(define (main)
  (display "Input a and b: ")
  (newline)
  (let ((a (read)) (b (read)))
    (display "The result is: ")
    (display (plus-abs a b))
    (newline)))

(main)
