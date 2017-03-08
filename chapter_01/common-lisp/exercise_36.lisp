;; The solution of exercise 1.36
;; Modify `fixed-point` so that it prints the sequence of approximations it
;; generates, using the `newline` and `display` primitives shown in
;; exercise 1.22. Then find a solution to x ^ x = 1000 by finding a fixed
;; point of x -> log(1000)/log(x). (Use Scheme's primitive `log` procedure,
;; which computes natural logarithms.) Compare the number of steps this
;; takes with and without average damping. (Note that you cannot start
;; `fixed-point` with a guess of 1, as this would cause division by
;; log(1) = 0.)
;;
;; -------- (above from SICP)
;;

;; The tolerance used in the fixed-point procedure
(defvar tolerance 0.00001)

;; Fix-point computation procedure
(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess step)
    (let ((next (funcall f guess)))
      (format t "Step[~d] ~c ~f ~%" step #\tab next)
      (if (close-enough? guess next)
          next
          (try next (+ 1 step)))))
  (try first-guess 1))

;; The procedure computes the root of equation: x ^ x = 1000
(defun root-exp-xx (first-guess)
  (fixed-point
   (lambda (x) (/ (log 1000) (log x)))
   first-guess))

;; The procedure also computes the root of equation: x ^ x = 1000, but
;; using average damping. This technique computes the result with much less
;; steps.
(defun root-exp-xx-ad (first-guess)
  (fixed-point
   (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
   first-guess))

(defun main ()
  (format t "Load this file and use ")
  (format t "`root-exp-xx`, `root-exp-xx-ad` procedure. ~%")
  (format t "Compute the root of equation: x ^ x = 1000 ")
  (format t "with first-guess 1.5. ~%No average damping: ~%")
  (format t "~a ~%"
          (root-exp-xx 1.5))
  (format t "With average damping: ~%")
  (format t "~a ~%"
          (root-exp-xx-ad 1.5)))

(main)


