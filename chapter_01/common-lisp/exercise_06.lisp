;; The solution of exercise 1.6
;; Square roots by Newton's method
;;
;; As a case in point, consider the problem of computing square roots.
;; We can define the square-root function as:
;;     square(x) = the y, such that y >= 0 and y^2 = x
;; This describes a perfectly legitimate mathematical function. We could
;; use it to recognize whether one number is the square root of another,
;; or to describe a procedure. Indeed, it tells us almost nothing about
;; how to actually find the square root of a given number. It will not
;; help matters to rephrase this definition in pseudo-Lisp:
;; 
;;     (define (sqrt x)
;;       (the y (and (>= y 0)
;;                   (= (square y) x))))
;;
;; This only begs the question.
;; -------- (above from SICP)
;;
;; How does one compute square roots? The most common way is to use
;; Newton's method of successive approximations.
;;

;; Compute the average value of two numbers
(defun average (x y)
  (/ (+ x y) 2))

;; The absolute value of a number
(defun abs (x)
  (if (< x 0) (- x) x))

;; A guess is improved by averaging it with the quotient of the radicand
;; and the old guess.
(defun improve (guess x)
  (average guess (/ x guess)))

;; It is important to point out what we mean by `good enough`. Here we
;; give a simple illustration (but it is not really a very good test).
;; The idea is to improve the answer until it is close enough so that its
;; square differs from the radicand by less than a predetermined tolerance
;; (here 0.000001 is set)
;; See exercise_07.scm to get a better good-enough? test.
(defun good-enough? (guess x)
  (< (abs (- (* guess guess) x)) 0.000001))

;; The basic strategy as a procedure
;; The thought is really simple, as we use recursive in definition
(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;; Now you can replace the default `if` with your `new-if`. Just simply
;; uncomment the following code and comment the original `sqrt-iter` code.
;;
;;     (defun new-if (predicate then-clause else-clause)
;;       (cond (predicate then-clause)
;;             (else else-clause)))
;;
;;     (defun sqrt-iter (guess x)
;;       (new-if (good-enough? guess x)
;;               guess
;;               (sqrt-iter (improve guess x)
;;                          x)))
;;
;; Compile or interpret this scheme file and look what will happen.
;; The compiler or interpreter will tell you error occurs: stack overflow.
;; Apparently it is caused by the `new-if` defind here. Actually the `if`
;; expression is a special form (restricted type of conditional that can
;; be used when there are precisely two cases in the case analysis). To
;; evaluate an `if` expression, the interpreter starts by evaluating the
;; <predicate> part. If the <predicate> evaluates to a true value, the
;; interpreter then evaluates the <consequence> and returns its value.
;; Otherwise it evaluates the <alternatice> and returns its value.
;; -------- (above from SICP)
;;
;; Thus only one of then-clause and else-clause will be evaluated in the
;; `if` special form. But if you define a `new-if` by `cond`, the circums-
;; tances changes. Actually then-clause and else-clause will be evaluated 
;; in `new-if`. You can test the following code:
;;
;;     (new-if #t
;;             (display "then-clause")
;;             (display "else-clause"))
;;
;; The interpreter will display two strings while only the first string if
;; using `the special form if`. So the `new-if` will destroy the normal
;; implementation of tail recursion, which leads to the situation where
;; the recursion of `new-if` and `sqrt-iter` exceeds the maximum recursion
;; depth of the interpreter and reports an error.
;;
          
;; Input a positive number and square its root.
(defun main ()
  (defvar init-value 1.0)
  (format t "Input a non-negative number: ")
  (let ((num (read)))
    (cond ((> num 0)
           (format t "The square root of this number is: ")
           (print (sqrt-iter init-value num)))
          ((= num 0) (print 0))
          (else (print "This number is negative!")))))

(main)


