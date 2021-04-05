;; The solution of exercise 1.21
;; Use the `smallest-divisor` procedure to find the smallest divisor of
;; each of the following numbers: 199, 1999, 19999
;;

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((= 0 (mod n test-divisor)) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))

(defun main ()
  (format t "Compute The smallest divisor. Input n: ")
  (let ((n (read)))
    (format t "[Smallest Divisor] ")
    (let ((sdiv (smallest-divisor n)))
      (format t "~d " sdiv)
      (if (= n sdiv)
          (format t "(Prime) ~%")
          (format t "~%")))))

;;
;; Test: find the smallest divisor of 199, 1999, 19999.
;;
;; Compute The smallest divisor.
;; Input n: 199
;; [Smallest Divisor] 199 (Prime)
;;
;; Compute The smallest divisor.
;; Input n: 1999
;; [Smallest Divisor] 1999 (Prime)
;;
;; Compute The smallest divisor.
;; Input n: 19999
;; [Smallest Divisor] 7
;;

(main)


