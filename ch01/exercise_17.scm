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
(define (double x) (* x 2))
(define (halve x) (/ x 2))

;; Multiply two integers (recursive process)
(define (multiply-rec a b)
  (cond ((= b 0) 0)
        ((= 0 (modulo b 2))
         (double (multiply-rec a (halve b))))
        ((= 1 (modulo b 2))
         (+ a (multiply-rec a (- b 1))))))

;;
;; Use trace to see the calls. For example, trace (multiply-rec 45 87):
;; trace: (multiply-rec 45 87)
;; trace: |  (multiply-rec 45 86)
;; trace: |  |  (halve 86)
;; trace: |  |  43
;; trace: |  |  (multiply-rec 45 43)
;; trace: |  |  |  (multiply-rec 45 42)
;; trace: |  |  |  |  (halve 42)
;; trace: |  |  |  |  21
;; trace: |  |  |  |  (multiply-rec 45 21)
;; trace: |  |  |  |  |  (multiply-rec 45 20)
;; trace: |  |  |  |  |  |  (halve 20)
;; trace: |  |  |  |  |  |  10
;; trace: |  |  |  |  |  |  (multiply-rec 45 10)
;; trace: |  |  |  |  |  |  |  (halve 10)
;; trace: |  |  |  |  |  |  |  5
;; trace: |  |  |  |  |  |  |  (multiply-rec 45 5)
;; trace: |  |  |  |  |  |  |  |  (multiply-rec 45 4)
;; trace: |  |  |  |  |  |  |  |  |  (halve 4)
;; trace: |  |  |  |  |  |  |  |  |  2
;; trace: |  |  |  |  |  |  |  |  |  (multiply-rec 45 2)
;; trace: |  |  |  |  |  |  |  |  |  |  (halve 2)
;; trace: |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  (multiply-rec 45 1)
;; trace: |  |  |  |  |  |  |  |  |  |  11> (multiply-rec 45 0)
;; trace: |  |  |  |  |  |  |  |  |  |  11< 0
;; trace: |  |  |  |  |  |  |  |  |  |  45
;; trace: |  |  |  |  |  |  |  |  |  (double 45)
;; trace: |  |  |  |  |  |  |  |  |  90
;; trace: |  |  |  |  |  |  |  |  (double 90)
;; trace: |  |  |  |  |  |  |  |  180
;; trace: |  |  |  |  |  |  |  225
;; trace: |  |  |  |  |  |  (double 225)
;; trace: |  |  |  |  |  |  450
;; trace: |  |  |  |  |  (double 450)
;; trace: |  |  |  |  |  900
;; trace: |  |  |  |  945
;; trace: |  |  |  (double 945)
;; trace: |  |  |  1890
;; trace: |  |  1935
;; trace: |  (double 1935)
;; trace: |  3870
;; trace: 3915
;;

(define (main)
  (display "Compute multiplication a * b.\nInput a, b: ")
  (let ((a (read)) (b (read)))
    (display "[Recursive process] ")
    (display a) (display " * ") (display b) (display " = ")
    (display (multiply-rec a b)))
  (newline))

(main)

