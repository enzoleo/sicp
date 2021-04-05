;; The solution of exercise 1.38
;; In 1737, the Swiss mathematician Leonhard Euler published a memoir De
;; Fractionibus Continuis, which included a continued fraction expansion
;; for `e - 2`, where e is the base of the natural logarithms. In this
;; fraction, the N_i are all 1, and the D_i are successively 1, 2, 1, 1,
;; 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac
;; procedure from exercise 1.37 to approximate `e`, based on Euler's
;; expansion.
;;
;; -------- (above from SICP)
;;

;; The `cont-frac` procedure as a iterative process
(define (cont-frac n d k)
  (define (iter counter result)
    (if (= 0 counter)
        result
        (iter (- counter 1)
              (/ (n counter)
                 (+ (d counter)
                    result)))))
  (iter k 0))

;; The iterative process to compute the approximate value of the base of
;; the natural logarithms `e` using Eular's formula.
(define (eular-expansion k)
  (+ 2.0
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= 2 (modulo i 3))
                      (/ (* 2.0 (+ i 1)) 3)
                      1.0))
                k)))

(define (main)
  (display "Load this file and use `eular-expansion` procedure.\n")
  (display "Compute `e` using k-term finite continued fraction.\n")
  (display "Input items k: ")
  (let ((k (read)))
    (display "[ Result (iterative) ] ")
    (display (eular-expansion k)) (newline)))

(main)


