;; The solution of exercise 2.1
;; Define a better version of make-rat that handles both positive and
;; negative arguments. Make-rat should normalize the sign so that if the
;; rational number is positive, both the numerator and denominator are
;; positive, and if the rational number is negative, only the numerator is
;; negative.
;;
;; -------- (above from SICP)
;;

;; Make a rational number with numerator and denominator
(define (make-rat n d)
  (let* ((g (gcd n d))
         (numerator (/ n g))
         (denominator (/ d g)))
    (if (< denominator 0)
        (cons (- numerator) (- denominator))
        (cons numerator denominator))))

;;
;; Note that we have another choice to define such procedures:
;;
;;     (define numer car)
;;     (define denom cdr)
;;
;; The first definition associates the name make-rat with the value of the
;; expression cons, which is the primitive procedure that constructs pairs.
;; Thus make-rat and cons are names for the same primitive constructor.
;; Defining selectors and constructors in this way is efficient: Instead of
;; `make-rat` calling cons, make-rat is cons, so there is only one
;; procedure called, not two, when make-rat is called. On the other hand,
;; doing this defeats debugging aids that trace procedure calls or put
;; breakpoints on procedure calls: You may want to watch make-rat being
;; called, but you certainly don't want to watch every call to cons.
;; (SICP)
;;
(define (numer x) (car x))
(define (denom x) (cdr x))

;; Simple operations
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;
;; Actually using the procedure defined above is available. Because the
;; result of function `gcd` has the same sign with the second argument,
;; which means the denominator of the result of `make-rat` is always
;; positive.

(define (main)
  (display "Load this file and use R - rationals")
  (newline))

(main)


