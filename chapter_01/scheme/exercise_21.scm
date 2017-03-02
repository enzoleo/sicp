;; The solution of exercise 1.21
;; Use the `smallest-divisor` procedure to find the smallest divisor of
;; each of the following numbers: 199, 1999, 19999
;;

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((= 0 (modulo n test-divisor)) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (main)
  (display "Compute The smallest divisor.\nInput n: ")
  (let ((n (read)))
    (display "[Smallest Divisor] ")
    (let ((sdiv (smallest-divisor n)))
      (display sdiv)
      (if (= n sdiv)
          (display " (Prime)")))
    (newline)))

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


