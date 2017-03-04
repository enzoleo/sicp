;; The solution of exercise 1.17
;; The exponentiation algorithms in this section are based on performing
;; exponentiation by means of repeated multiplication. In a similar way,
;; one can perform interger multiplication by means of repeated addition.
;; The following multiplication procedure (in which it is assumed that our
;; language can only add, not multiply) is analogous to the expt procedure:
;;
;;     (define (* a b)
;;       (if (= b 0)
;;           0
;;           (+ a (* a (- b 1)))))
;;
;; This algorithm takes a number of steps that is linear in b. Now suppose
;; we include, together with addition, operations `double`, which doubles
;; an integer, and `halve`, which divides an (even) integer by 2. Using
;; these, design a multiplication procedure analogous to `fast-expt` that
;; uses a logarithmic number of steps.
;;
;; -------- (above from SICP)
;;

;; Definition of simple procedures
;; Try not using some ordinary names like `double`, `halve` or `abs`, which
;; are very likely default names in Common-Lisp
(defun dbl (x) (* x 2))
(defun hlv (x) (/ x 2))

;; Multiply two integers (recursive process)
(defun multiply-rec (a b)
  (cond ((= b 0) 0)
        ((= 0 (mod b 2))
         (dbl (multiply-rec a (hlv b))))
        ((= 1 (mod b 2))
         (+ a (multiply-rec a (- b 1))))))

;;
;; Use trace to see the calls. For example, trace (multiply-rec 45 87):
;; * (trace multiply-rec)
;;
;; (MULTIPLY-REC)
;; * (multiply-rec 45 87)
;;   0: (MULTIPLY-REC 45 87)
;;     1: (MULTIPLY-REC 45 86)
;;       2: (MULTIPLY-REC 45 43)
;;         3: (MULTIPLY-REC 45 42)
;;           4: (MULTIPLY-REC 45 21)
;;             5: (MULTIPLY-REC 45 20)
;;               6: (MULTIPLY-REC 45 10)
;;                 7: (MULTIPLY-REC 45 5)
;;                   8: (MULTIPLY-REC 45 4)
;;                     9: (MULTIPLY-REC 45 2)
;;                       10: (MULTIPLY-REC 45 1)
;;                         11: (MULTIPLY-REC 45 0)
;;                         11: MULTIPLY-REC returned 0
;;                       10: MULTIPLY-REC returned 45
;;                     9: MULTIPLY-REC returned 90
;;                   8: MULTIPLY-REC returned 180
;;                 7: MULTIPLY-REC returned 225
;;               6: MULTIPLY-REC returned 450
;;             5: MULTIPLY-REC returned 900
;;           4: MULTIPLY-REC returned 945
;;         3: MULTIPLY-REC returned 1890
;;       2: MULTIPLY-REC returned 1935
;;     1: MULTIPLY-REC returned 3870
;;   0: MULTIPLY-REC returned 3915
;; 3915
;;

(defun main ()
  (format t "Compute multiplication a * b.~%Input a, b: ")
  (let ((a (read)) (b (read)))
    (format t "[Recursive process] ~a * ~a = ~a ~%"
            a b (multiply-rec a b))))

(main)

