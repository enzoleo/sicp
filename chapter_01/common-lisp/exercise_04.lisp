;; The solution of exercise 1.4
;; Observe that our model of evaluation allows for combinations whose
;; operators are compound expressions. Use this observation to describe
;; the behavior of the following procedure.

(defun plus-abs (a b)
  (funcall (if (> b 0) #'+ #'-) a b))

;; Note how the function works.
;; The expression above is equivalent to the math equation: a + abs(b)
;; (if (> b 0) + -) returns function `+` if b > 0 is true.
;; Notice that `+` and `-` may be operators in some other programming
;; language, but in Scheme they are functions instead.

(defun main ()
  (format t "Input a and b: ")
  (let ((a (read)) (b (read)))
    (format t "The result is: ~a ~%" (plus-abs a b))))

(main)
