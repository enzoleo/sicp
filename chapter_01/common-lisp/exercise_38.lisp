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
(defun cont-frac (n d k)
  (defun iter (counter result)
    (if (= 0 counter)
        result
        (iter (- counter 1)
              (/ (funcall n counter)
                 (+ (funcall d counter)
                    result)))))
  (iter k 0))

;; The iterative process to compute the approximate value of the base of
;; the natural logarithms `e` using Eular's formula.
(defun eular-expansion (k)
  (+ 2.0
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= 2 (mod i 3))
                      (/ (* 2.0 (+ i 1)) 3)
                      1.0))
                k)))

(defun main ()
  (format t "Load this file and use `eular-expansion` procedure. ~%")
  (format t "Compute `e` using k-term finite continued fraction. ~%")
  (format t "Input items k: ")
  (let ((k (read)))
    (format t "[ Result (iterative) ] ~f ~%"
            (eular-expansion k))))

(main)


