;; The solution of exercise 1.11
;; A function f is defined by the rule that f(n) = n if n < 3 and f(n) =
;; f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3) if n >= 3. Write a procedure
;; that computes f by means of a recursive process. Write a procedure that
;; computes f by means of an iterative process
;;

(defun recursive-f (n)
  (if (<= n 3)
      n
      (+ (recursive-f (- n 1))
         (* 2 (recursive-f (- n 2)))
         (* 3 (recursive-f (- n 3))))))

(defun f-iter (a b c count)
  (if (= count 1)
      c
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(defun iterative-f (n)
  (f-iter 3 2 1 n))
  
(defun main ()
  (format t "Input n: ")
  (let ((n (read)))
    (format t "[iterative process] f(~d) = ~d ~%" n (iterative-f n))
    (format t "[recursive process] f(~d) = ~d ~%" n (recursive-f n))))

(main)


