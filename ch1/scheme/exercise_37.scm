;; The solution of exercise 1.37
;; [a] An infinite continued fraction is an expression of the form:
;;
;;                       N_1
;;         f = -------------------------
;;                           N_2
;;             D_1 + -------------------
;;                              N_3
;;                    D_2 + ------------
;;                                  N_4
;;                           D_3 + -----
;;                                  ...
;;
;;     As an example, one can show that the infinite continued fraction
;;     expansion with the N_i and the D_i all equal to 1 produces 1 / \phi,
;;     where \phi is the golden ratio (described in section 1.2.2). One way
;;     to approximate an infinite continued fraction is to truncate the
;;     expansion after a given number of terms. Such a truncation -- a
;;     so-called `k-term finite continued fraction` -- has the form:
;;
;;                 N_1
;;         --------------------
;;                    N_2
;;         D_1 + --------------
;;                        N_k
;;                ...  + -----
;;                        D_k
;;
;;     Suppose that `n` and `d` are procedures of one argument (the term
;;     index i) that return the N_i and D_i of the terms of the continued
;;     fraction. Define a procedure cont-frac such that evaluating `(cont-
;;     frac n d k)` computes the value of the k-term finite continued
;;     fraction. Check your procedure by approximating 1 / \phi using
;;
;;         (cont-frac (lambda (i) 1.0)
;;                    (lambda (i) 1.0)
;;                    k)
;;
;;     for successive values of `k`. How large must you make k in order to
;;     get an approximation that is accurate to 4 decimal places?
;;
;; [b] If your cont-frac procedure generates a recursive process, write
;;     one that generates an iterative process. If it generates an
;;     iterative process, write one that generates a recursive process.
;;
;; -------- (above from SICP)
;;

;; The `cont-frac` procedure as a recursive process
(define (cont-frac-rec n d k)
  (define (iter counter)
    (/ (n counter)
       (+ (d counter)
          (if (= k counter)
              0
              (iter (+ 1 counter))))))
  (iter 1))

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

;; The recursive process to compute golden ratio
(define (golden-ratio-rec k)
  (cont-frac-rec (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 k))

;; The iterative process to compute golden ratio
(define (golden-ratio k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

;; At least 4 items needed to get an approximation that is accurate to 4
;; decimal places:
;;
;; Input items k: 9
;; [  The exact value   ] 0.6180339887498949
;; [ Result (iterative) ] 0.6181818181818182
;; [ Result (recursive) ] 0.6181818181818182
;;
;; Input items k: 10
;; [  The exact value   ] 0.6180339887498949
;; [ Result (iterative) ] 0.6179775280898876
;; [ Result (recursive) ] 0.6179775280898876
;;
;; Input items k: 11
;; [  The exact value   ] 0.6180339887498949
;; [ Result (iterative) ] 0.6180555555555556
;; [ Result (recursive) ] 0.6180555555555556
;;
(define (main)
  (display "Load this file and use ")
  (display "`cont-frac-rec`, `cont-frac` procedure.\n")
  (display "Compute 1 / phi using k-term finite continued fraction, ")
  (display "where phi is the golden ratio. Simply use `golden-ratio`.\n")
  (display "Input items k: ")
  (let ((k (read)))
    (display "[  The exact value   ] ")
    (display (/ (- (sqrt 5) 1) 2)) (newline)
    (display "[ Result (iterative) ] ")
    (display (golden-ratio k)) (newline)
    (display "[ Result (recursive) ] ")
    (display (golden-ratio-rec k)) (newline)))

(main)


