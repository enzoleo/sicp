;; The solution of exercise 1.18
;; Using the results of exercises 1.16 and 1.17, devise a procedure that
;; generates an iterative process for multiplying two integers in terms of
;; adding, doubling, and halving and uses a logarithmic number of steps.
;;
;; -------- (above from SICP)
;;

;; Definition of simple procedures
(defun dbl (x) (* x 2))
(defun hlv (x) (/ x 2))

(defun compute-iter (ans a b)
  (cond ((= b 0) ans)
        ((= 0 (mod b 2))
         (compute-iter ans (dbl a) (hlv b)))
        ((= 1 (mod b 2))
         (compute-iter (+ ans a) a (- b 1)))))

;; Multiply two integers (iterative process)
(defun multiply-itr (a b)
    (compute-iter 0 a b))

;;
;; Use trace to see the calls. For example, trace (multiply-itr 45 87):
;; * (trace compute-iter)
;;
;; (COMPUTE-ITER)
;; * (compute-iter 0 45 87)
;;   0: (COMPUTE-ITER 0 45 87)
;;     1: (COMPUTE-ITER 45 45 86)
;;       2: (COMPUTE-ITER 45 90 43)
;;         3: (COMPUTE-ITER 135 90 42)
;;           4: (COMPUTE-ITER 135 180 21)
;;             5: (COMPUTE-ITER 315 180 20)
;;               6: (COMPUTE-ITER 315 360 10)
;;                 7: (COMPUTE-ITER 315 720 5)
;;                   8: (COMPUTE-ITER 1035 720 4)
;;                     9: (COMPUTE-ITER 1035 1440 2)
;;                       10: (COMPUTE-ITER 1035 2880 1)
;;                         11: (COMPUTE-ITER 3915 2880 0)
;;                         11: COMPUTE-ITER returned 3915
;;                       10: COMPUTE-ITER returned 3915
;;                     9: COMPUTE-ITER returned 3915
;;                   8: COMPUTE-ITER returned 3915
;;                 7: COMPUTE-ITER returned 3915
;;               6: COMPUTE-ITER returned 3915
;;             5: COMPUTE-ITER returned 3915
;;           4: COMPUTE-ITER returned 3915
;;         3: COMPUTE-ITER returned 3915
;;       2: COMPUTE-ITER returned 3915
;;     1: COMPUTE-ITER returned 3915
;;   0: COMPUTE-ITER returned 3915
;; 3915
;;

(defun main ()
  (format t "Compute multiplication a * b.~%Input a, b: ")
  (let ((a (read)) (b (read)))
    (format t "[Iterative process] ~a * ~a = ~a ~%"
            a b (multiply-itr a b))))

(main)


