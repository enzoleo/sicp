;; The solution of exercise 1.3
;; Define a procedure that takes three numbers as arguments and returns
;; the sum of the squares of the two larger numbers

;; Input three numbers from keyboard
;; Return the sum of the squares of the two larger numbers
(defun get-largest-sum-square (a b c)
  (if (> a b)
      (if (> b c)
          (+ (* a a) (* b b))
          (+ (* a a) (* c c)))
      (if (> a c)
          (+ (* a a) (* b b))
          (+ (* c c) (* b b)))))

(defun main ()
  (format t "Input three numbers: ")
  (let ((a (read)) (b (read)) (c (read)))
    (format t "The sum of the squares of the two larger numbers: ~a ~%"
            (get-largest-sum-square a b c))))

(main)

