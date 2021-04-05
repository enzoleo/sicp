;; The solution of exercise 1.31
;; [a] The `sum` procedure is only the simplest of a vast number of similar
;;     abstractions that can be captured as higher-order procedures. Write
;;     an analogous procedure called `product` that returns the product of
;;     the values of a function at points over a given range. Show how to
;;     define factorial in terms of product. Also use `product` to compute
;;     approximations to using the Wallis formula:
;;
;;     PI     2 * 4 * 4 * 6 * 6 * 8 * 8 * 10 * ...
;;    ---- = --------------------------------------
;;     4      3 * 3 * 5 * 5 * 7 * 7 * 9 * 9  * ...
;;
;; [b] If your product procedure generates a recursive process, write one
;;     that generates an iterative process. If it generates an iterative
;;     process, write one that generates a recursive process.
;;
;; -------- (above from SICP)
;;

;; The square procedure
(defun square (x) (* x x))

;; New abstract product procedure (iterative process)
(defun product (term a next b)
  (defun iter (m result)
    (if (> m b)
        result
        (iter (funcall next m) (* result (funcall term m)))))
  (iter a 1))

;; New abstract product procedure (recursive process)
(defun product-rec (term a next b)
  (if (> a b)
      1
      (* (funcall term a)
         (product-rec term (funcall next a) next b))))

;; The procedure to compute the value of PI approximately using the famous
;; John-Wallis formula. (This is an iterative process)
(defun wallis-pi (n)
  ;; Here we convert the exact rational number to an inexact float number.
  ;; Because we do not need the computer to compute the exact rational
  ;; approximate value. Just float number is Ok, and it can increase the
  ;; computing speed at the same time.
  (defun term (m)
    (/ (- (square m) 1.0) (square m)))
  (* 4.0 (product #'term
                  3
                  (lambda (x) (+ x 2))
                  (+ 1 (* 2 n)))))

;; The procedure to compute the value of PI approximately using the famous
;; John-Wallis formula. (This is an recursive process)
(defun wallis-pi-rec (n)
  (defun term (m)
    (/ (- (square m) 1.0) (square m)))
  (* 4.0 (product-rec #'term
                      3
                      (lambda (x) (+ x 2))
                      (+ 1 (* 2 n)))))

;;
;; Not every implementation has the procedure `runtime`. You can
;; use alternatives for different interpreter.
;;
;; [SBCL]
;;     (defun runtime () (get-internal-real-time))
;;
(defun runtime() (get-internal-real-time))

;; Compute the runtime of @search-for-next-primes
(defun wallis-runtime (n)
  (defun clause-runtime (start-time)
    (format t "PI: ~a ~%" (wallis-pi n))
    (format t "runtime: ~a ~%" (- (runtime) start-time)))
  (clause-runtime (runtime)))

(defun main ()
  (format t "Load this file and use `wallis-pi` procedure. ~%")
  (format t "We compute PI using wallis formula with 1000000 terms. ~%")
  (format t "Default: iterative process. ~%")
  (format t "Use `wallis-pi-rec` procedure for recursive process. ~%")
  (wallis-runtime 1000000))

(main)


