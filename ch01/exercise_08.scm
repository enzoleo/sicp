;; The solution of exercise 1.8
;; Newton's method for cube roots is based on the fact that if y is an
;; approximation to the cube root of x, then a better approximation is
;; given by the value (x / y^2 + 2y) / 3.
;;
;; You can prove that give an initial value 1.0, then the formula above
;; defines a sequence of approximate cube roots of x and it converges to
;; the cube roots of x. So we use this formula to implement a cube-root
;; procedure analogous to the square-root procedure.
;; 

;; The absolute value of a number
(define (abs x)
  (if (< x 0) (- x) x))

;; A guess is improved by the given iteration formula
(define (improve guess x)
  (/ (+ (/ x (* guess guess)) guess guess) 3))

;; It is important to point out what we mean by `good enough`. Here we
;; give a simple illustration (different from the test in exercise 1.6).
;; The idea is to improve the answer until it is close enough so that the
;; difference between new guess and old guess is really small, compared
;; with the old guess. So we consider the quotient. (here we compare this
;; quotient to 0.0001)
;;
;; ATTENTION: we update this procedure `good-enough?`, compared with that
;; in exercise 1.6 and 1.7 because we need to consider negative numbers for
;; all real numbers have cube roots.
(define (good-enough? new-guess old-guess)
  (< (/ (abs (- new-guess old-guess))
        (abs old-guess)) 0.0001))

;; The basic strategy as a procedure
;; The thought is really simple, as we use recursive in definition
(define (curt-iter guess x)
  (let ((new-guess (improve guess x)))
    (if (good-enough? new-guess guess)
        new-guess
        (curt-iter (improve new-guess x)
                   x))))
          
;; Input a positive number and compute its cube root.
(define (main)
  (define init-value 1.0)
  (display "Input a real number: ")
  (let ((num (read)))
    (if (= num 0)
      (display 0)
      (begin
        (display "The cube root of this number is: ")
        (display (curt-iter init-value num)))))
  (newline))

(main)


