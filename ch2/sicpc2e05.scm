;; The solution of exercise 2.5
;; Show that we can represent pairs of nonnegative integers using only
;; numbers and arithmetic operations if we represent the pair a and b as
;; the integer that is the product 2 ^ a * 3 ^ b . Give the corresponding
;; definitions of the procedures cons, car, and cdr.
;;
;; -------- (above from SICP)
;;

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (mod-factor n factor counter)
    (if (= 0 (modulo n factor))
        (mod-factor (/ n factor) factor (+ counter 1))
        counter))

(define (car z) (mod-factor z 2 0))
(define (cdr z) (mod-factor z 3 0))

(define (main)
  (display "Load this file and use `cons`, `car` and  `cdr`.")
  (newline))

(main)


