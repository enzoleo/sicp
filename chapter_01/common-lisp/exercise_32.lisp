;; The solution of exercise 1.32
;; [a] Show that sum and product (exercise 1.31) are both special cases of
;;     a still more general notion called accumulate that combines a
;;     collection of terms, using some general accumulation function:
;;     (accumulate combiner null-value term a next b) Accumulate takes as
;;     arguments the same term and range specifications as sum and product,
;;     together with a combiner procedure (of two arguments) that specifies
;;     how the current term is to be combined with the accumulation of the
;;     preceding terms and a null-value that specifies what base value to
;;     use when the terms run out. Write accumulate and show how sum and
;;     product can both be defined as simple calls to accumulate.
;;
;; [b] If your accumulate procedure generates a recursive process, write
;;     one that generates an iterative process. If it generates an
;;     iterative process, write one that generates a recursive process.
;;
;; -------- (above from SICP)
;;

;; The square procedure
(defun square (x) (* x x))

;; The abstract accumulate procedure (iterative and recursive process)
(defun accumulate (combiner null-value term a next b)
  (defun iter (m result)
    (if (> m b)
        result
        (iter (funcall next m)
              (funcall combiner result (funcall term m)))))
  (iter a null-value))

(defun accumulate-rec (combiner null-value term a next b)
  (if (> a b)
      null-value
      (funcall combiner
               (funcall term a)
               (accumulate-rec combiner
                               null-value
                               term
                               (funcall next a) next b))))

;; New abstract sum procedure (iterative and recursive process)
;; Use accumulate procedure (combiner = "+" procedure)
(defun sum (term a next b)
  (accumulate #'+ 0 term a next b))
(defun sum-rec (term a next b)
  (accumulate-rec #'+ 0 term a next b))

;; New abstract product procedure (iterative and recursive process)
;; Use accumulate procedure (combiner = "*" procedure)
(defun product (term a next b)
  (accumulate #'* 1 term a next b))
(defun product-rec (term a next b)
  (accumulate-rec #'* 1 term a next b))

;; Wallis formula is used to test `product`
(defun wallis-pi (n)
  (defun term (m)
    (/ (- (square m) 1.0) (square m)))
  (* 4.0 (product #'term
                  3
                  (lambda (x) (+ x 2))
                  (+ 1 (* 2 n)))))

;; Leibniz formula is used to test `sum`
(defun leibniz-pi (n)
  (* 8.0
     (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
          1
          (lambda (x) (+ x 4))
          n)))

(defun main ()
  (format t "Load this file and use `accumulate` procedure. ~%")
  (format t "We use `accumulate` to define `sum` and `product`. ~%")
  (format t "Use `sum`, `sum-rec`, `product` and `product-rec`. ~%")
  (format t "Test product: [Wallis formula ]: PI = ~a ~%"
          (wallis-pi 10000000))
  (format t "Test sum    : [Leibniz formula]: PI = ~a ~%"
          (leibniz-pi 10000000))
  (format t "(We compute 10 ^ 7 terms in the two tests above)~%"))

(main)


