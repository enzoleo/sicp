;; The solution of exercise 1.2
;; Translate the given expression into prefix form

(defvar result
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

(defun main ()
  (print result))

(main)
