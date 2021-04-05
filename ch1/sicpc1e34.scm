;; The solution of exercise 1.34
;; Suppose we define the procedure:
;;
;;     (define (f g)
;;       (g 2))
;;
;; Then we have:
;;
;;     > (f square)
;;     > 4
;;
;;     > (f (lambda (z) (* z (+ z 1))))
;;     > 6
;;
;; What happens if we (perversely) ask the interpreter to evaluate the
;; combination (f f)? Explain.
;;
;; -------- (above from SICP)
;;

(define (f g)
  (g 2))

;;
;; We test `(f f)` in guile:
;;
;;     > (f f)
;;     ERROR: In procedure 2:
;;     ERROR: Wrong type to apply: 2
;;
;; and we also test `(f f)` in mit-scheme:
;;
;;     ]=> (f f)
;;
;;     The object 2 is not applicable.
;;     To continue, call RESTART with an option number:
;;     (RESTART 2) => Specify a procedure to use in its place.
;;     (RESTART 1) => Return to read-eval-print level 1.
;;
;; Actually the interpreter will make it to a weird step: (2 2). But 2 is
;; not a valid procedure. So the interpreter will report an error "The
;; object 2 is not applicable" or "Wrong type to apply: 2".
;;

(define (main)
  (display "Load this file and use `f` procedure for test.\n")
  (newline))

(main)


