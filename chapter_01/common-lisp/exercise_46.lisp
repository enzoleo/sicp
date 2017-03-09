;; The solution of exercise 1.46
;; Several of the numerical methods described in this chapter are instances
;; of an extremely general computational strategy known as `iterative
;; improvement`. Iterative improvement says that, to compute something,
;; we start with an initial guess for the answer, test if the guess is good
;; enough, and otherwise improve the guess and continue the process using
;; the improved guess as the new guess. Write a procedure `iterative-
;; improve` that takes two procedures as arguments: a method for telling
;; whether a guess is good enough and a method for improving a guess.
;; `Iterative-improve` should return as its value a procedure that takes a
;; guess as argument and keeps improving the guess until it is good enough.
;; Rewrite the `sqrt` procedure of section 1.1.7 and the `fixed-point`
;; procedure of section 1.3.3 in terms of `iterative-improve`.
;;
;; -------- (above from SICP)
;;

;; The abstract iterative-improve procedure
(defun iterative-improve (close-enough? improve)
  (lambda (first-guess)
    (defun try (guess)
      (let ((next (funcall improve guess)))
        (if (funcall close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

;; The tolerance used in the fixed-point procedure
(defvar tolerance 0.00001)

;; Fix-point computation procedure
(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (funcall (iterative-improve #'close-enough? f) first-guess))

;; The new `new-sqrt` procedure
(defun new-sqrt (x)
  (defun close-enough? (new-guess old-guess)
    (< (/ (abs (- new-guess old-guess))
          old-guess) 0.0001))
  (defun improve (guess)
    (/ (+ guess (/ x guess)) 2))
  (funcall (iterative-improve #'close-enough? #'improve) 1.0))

;;
;; TEST: (fixed-point cos 1.0)
;; TEST RESULT: 0.7390823 
;; TEST: (new-sqrt 576)
;; TEST RESULT: 24.0
;;

(defun main ()
  (format t "Load this file and use `fixed-point` and `sqrt` procedure.~%")
  (format t "TEST: (fixed-point cos 1.0)~%TEST RESULT: ~f ~%"
          (fixed-point #'cos 1.0))
  (format t "TEST: (new-sqrt 576)~%TEST RESULT: ~f ~%"
          (new-sqrt 576)))

(main)


