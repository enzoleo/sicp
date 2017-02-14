;; The solution of exercise 1.1
;; Give the result printed by the interpreter in response to each express-
;; ion. Assume that the sequence is to be evaluated in the order in which
;; it is presented.

(define a 3)
(define b (+ a 1))
(define (display-newline clause)
  (display clause)
  (newline))

(define (main)
  (display-newline 10)
  (display-newline (+ 5 3 4))
  (display-newline (- 9 1))
  (display-newline (/ 6 2))
  (display-newline (+ (* 2 4) (- 4 6)))
  (display-newline (+ a b (* a b)))
  (display-newline (= a b))
  (display-newline
   (if (and (> b a) (< b (* a b)))
       b
       a))
  (display-newline
   (cond ((= a 4) 6)
         ((= b 4) (+ 6 7 a))
         (else 25)))
  (display-newline (+ 2 (if (> b a) b a)))
  (display-newline
   (* (cond ((> a b) a)
            ((< a b) b)
            (else -1))
      (+ a 1))))

(main)

