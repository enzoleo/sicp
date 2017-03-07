;; The solution of exercise 1.34
;; Suppose we define the procedure:
;;
;;     (defun f (g)
;;       (funcall g 2))
;;
;; Then we have:
;;
;;     * (f #'square)
;;     4
;;
;;     * (f (lambda (z) (* z (+ z 1))))
;;     6
;;
;; What happens if we (perversely) ask the interpreter to evaluate the
;; combination (f f)? Explain.
;;
;; -------- (above from SICP)
;;

(defun f (g)
  (funcall g 2))

;;
;; We test `(f f)` in sbcl:
;;
;;     * (f f)
;;
;;     debugger invoked on a UNBOUND-VARIABLE in thread
;;     #<THREAD "main thread" RUNNING {100399C7B3}>:
;;     The variable F is unbound.
;;
;;     Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.
;;
;;     restarts (invokable by number or by possibly-abbreviated name):
;;     0: [ABORT] Exit debugger, returning to top level.
;;
;;     (SB-INT:SIMPLE-EVAL-IN-LEXENV F #<NULL-LEXENV>)
;;
;; Actually the interpreter will make it to a weird step: (2 2). But 2 is
;; not a valid procedure. So the interpreter will report an error.
;;

(defun main ()
  (format t "Load this file and use `f` procedure for test. ~%"))

(main)


