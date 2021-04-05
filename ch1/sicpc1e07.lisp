;; The solution of exercise 1.7
;; Square roots by Newton's method
;; The `good-enough?` test in exercise 1.6 will not be very effective for
;; finding the square roots of very small numbers (because the number used
;; for comparison is always a positive number which means there always
;; exists smaller numbers). Also, in real computers, arithmetic operations
;; are almost always performed with limited precision. This makes our test
;; inadequate for very large numbers.
;;
;; An alternative strategy for implementing `good-enough?` is to watch how
;; `guess` changes from one iteration to the next and to stop when the
;; change is a very small fraction of the guess. here we simply design a
;; square-root procedure that uses this kind of end test.
;; 

;; Compute the average value of two numbers
(defun average (x y)
  (/ (+ x y) 2))

;; The absolute value of a number
(defun fabs (x)
  (if (< x 0) (- x) x))

;; A guess is improved by averaging it with the quotient of the radicand
;; and the old guess.
(defun improve (guess x)
  (average guess (/ x guess)))

;; It is important to point out what we mean by `good enough`. Here we
;; give a simple illustration (different from the test in exercise 1.6).
;; The idea is to improve the answer until it is close enough so that the
;; difference between new guess and old guess is really small, compared
;; with the old guess. So we consider the quotient. (here we compare this
;; quotient to 0.0001)
(defun good-enough? (new-guess old-guess)
  (< (/ (fabs (- new-guess old-guess))
        old-guess) 0.0001))

;; The basic strategy as a procedure
;; The thought is really simple, as we use recursive in definition
(defun sqrt-iter (guess x)
  (let ((new-guess (improve guess x)))
    (if (good-enough? new-guess guess)
        new-guess
        (sqrt-iter (improve new-guess x)
                   x))))
          
;; Input a positive number and square its root.
(defun main ()
  (let ((init-value 1.0))
    (format t "Input a non-negative number: ")
    (let ((num (read)))
      (cond ((> num 0)
             (format t "The square root of this number is: ")
             (print (sqrt-iter init-value num)))
            ((= num 0) (print 0))
            (t (print "This number is negative!"))))))

(main)

