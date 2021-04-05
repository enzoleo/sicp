;; The solution of exercise 2.5
;; Show that we can represent pairs of nonnegative integers using only
;; numbers and arithmetic operations if we represent the pair a and b as
;; the integer that is the product 2 ^ a * 3 ^ b . Give the corresponding
;; definitions of the procedures cons, car, and cdr.
;;
;; -------- (above from SICP)
;;

(defun new-cons (a b)
  (* (expt 2 a)
     (expt 3 b)))

(defun mod-factor (n factor counter)
    (if (= 0 (mod n factor))
        (mod-factor (/ n factor) factor (+ counter 1))
        counter))

(defun new-car (z) (mod-factor z 2 0))
(defun new-cdr (z) (mod-factor z 3 0))

(defun main ()
  (format t "Load this file and use `cons`, `car` and  `cdr`.~%"))

(main)


