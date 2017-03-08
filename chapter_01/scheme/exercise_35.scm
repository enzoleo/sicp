;; The solution of exercise 1.35
;; Show that the golden ratio \phi (section 1.2.2) is a fixed point of the
;; transformation: x -> 1 + 1 / x, and use this fact to compute \phi by
;; means of the fixed-point procedure.
;;
;; -------- (above from SICP)
;;

;; The tolerance used in the fixed-point procedure
(define tolerance 0.00001)

;; Fix-point computation procedure
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; The procedure computes the golden ratio
(define (golden-ratio first-guess)
  (fixed-point
   (lambda (x) (+ 1 (/ 1 x)))
   first-guess))

(define (main)
  (display "Load this file and use `golden-ratio` procedure.\n")
  (display "We compute the golden ratio with first-guess 1.0.\n")
  (display "GOLDEN-RATIO = ")
  (display (golden-ratio 1.0))
  (newline))

(main)


