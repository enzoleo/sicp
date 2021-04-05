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
(define (iterative-improve close-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

;; The tolerance used in the fixed-point procedure
(define tolerance 0.00001)

;; Fix-point computation procedure
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

;; The new `sqrt` procedure
(define (sqrt x)
  (define (close-enough? new-guess old-guess)
    (< (/ (abs (- new-guess old-guess))
          old-guess) 0.0001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve close-enough? improve) 1.0))

;;
;; TEST: (fixed-point cos 1.0)
;; TEST RESULT: 0.7390822985224024
;; TEST: (sqrt 576)
;; TEST RESULT: 24.000000025758766
;;

(define (main)
  (display "Load this file and use `fixed-point` and `sqrt` procedure.\n")
  (display "TEST: (fixed-point cos 1.0)\n")
  (display "TEST RESULT: ")
  (display (fixed-point cos 1.0))
  (newline)
  (display "TEST: (sqrt 576)\n")
  (display "TEST RESULT: ")
  (display (sqrt 576))
  (newline))

(main)


