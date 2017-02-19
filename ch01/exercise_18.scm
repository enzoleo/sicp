;; The solution of exercise 1.18
;; Using the results of exercises 1.16 and 1.17, devise a procedure that
;; generates an iterative process for multiplying two integers in terms of
;; adding, doubling, and halving and uses a logarithmic number of steps.
;;
;; -------- (above from SICP)
;;

;; Definition of simple procedures
(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (compute-iter ans a b)
  (cond ((= b 0) ans)
        ((= 0 (modulo b 2))
         (compute-iter ans (double a) (halve b)))
        ((= 1 (modulo b 2))
         (compute-iter (+ ans a) a (- b 1)))))

;; Multiply two integers (iterative process)
(define (multiply-itr a b)
  (compute-iter 0 a b))

;;
;; Use trace to see the calls. For example, trace (multiply-itr 45 87):
;; trace: (multiply-itr 45 87)
;; trace: (compute-iter 0 45 87)
;; trace: (compute-iter 45 45 86)
;; trace: |  (double 45)
;; trace: |  90
;; trace: |  (halve 86)
;; trace: |  43
;; trace: (compute-iter 45 90 43)
;; trace: (compute-iter 135 90 42)
;; trace: |  (double 90)
;; trace: |  180
;; trace: |  (halve 42)
;; trace: |  21
;; trace: (compute-iter 135 180 21)
;; trace: (compute-iter 315 180 20)
;; trace: |  (double 180)
;; trace: |  360
;; trace: |  (halve 20)
;; trace: |  10
;; trace: (compute-iter 315 360 10)
;; trace: |  (double 360)
;; trace: |  720
;; trace: |  (halve 10)
;; trace: |  5
;; trace: (compute-iter 315 720 5)
;; trace: (compute-iter 1035 720 4)
;; trace: |  (double 720)
;; trace: |  1440
;; trace: |  (halve 4)
;; trace: |  2
;; trace: (compute-iter 1035 1440 2)
;; trace: |  (double 1440)
;; trace: |  2880
;; trace: |  (halve 2)
;; trace: |  1
;; trace: (compute-iter 1035 2880 1)
;; trace: (compute-iter 3915 2880 0)
;; trace: 3915
;;

(define (main)
  (display "Compute multiplication a * b.\nInput a, b: ")
  (let ((a (read)) (b (read)))
    (display "[Iterative process] ")
    (display a) (display " * ") (display b) (display " = ")
    (display (multiply-itr a b)))
  (newline))

(main)


