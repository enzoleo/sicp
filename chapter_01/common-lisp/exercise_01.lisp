;; The solution of exercise 1.1
;; Give the result printed by the interpreter in response to each express-
;; ion. Assume that the sequence is to be evaluated in the order in which
;; it is presented.

;; Define some variables used later
(defvar a 3)
(defvar b (+ a 1))

(defun main ()
  (print 10)
  (print (+ 5 3 4))
  (print (- 9 1))
  (print (/ 6 2))
  (print (+ (* 2 4) (- 4 6)))
  (print (+ a b (* a b)))
  (print (= a b))
  (print (if (and (> b a) (< b (* a b))) b a))
  (print (cond ((= a 4) 6)
               ((= b 4) (+ 6 7 a))
               (else 25)))
  (print (+ 2 (if (> b a) b a)))
  (print (* (cond ((> a b) a)
                  ((< a b) b)
                  (else -1))
            (+ a 1))))

(main)

