;; The solution of exercise 1.35
;; Show that the golden ratio \phi (section 1.2.2) is a fixed point of the
;; transformation: x -> 1 + 1 / x, and use this fact to compute \phi by
;; means of the fixed-point procedure.
;;
;; -------- (above from SICP)
;;

;; The tolerance used in the fixed-point procedure
(defvar tolerance 0.00001)

;; Fix-point computation procedure
(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; The procedure computes the golden ratio
(defun golden-ratio (first-guess)
  (fixed-point
   (lambda (x) (+ 1 (/ 1 x)))
   first-guess))

(defun main ()
  (format t "Load this file and use `golden-ratio` procedure. ~%")
  (format t "We compute the golden ratio with first-guess 1.0. ~%")
  (format t "GOLDEN-RATIO = ~f ~%" (golden-ratio 1.0)))

(main)
