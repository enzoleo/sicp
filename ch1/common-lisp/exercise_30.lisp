;; The solution of exercise 1.30
;; The sum procedure above generates a linear recursion. The procedure
;; can be rewritten so that the sum is performed iteratively. Show how
;; to do this process.
;;
;; -------- (above from SICP)
;;

;; Compute cube of a number
(defun cube (x) (* x x x))

;; New abstrack summation procedure (iterative process)
(defun sum (term a next b)
  (defun iter (m result)
    (if (> m b)
        result
        (iter (funcall next m) (+ result (funcall term m)))))
  (iter a 0))

;; The simpson integral procedure
(defun simpson (f a b n)
  (let ((h (/ (- b a) n)))
    (defun fun-iter (k)
        (funcall f (+ a (* k h))))
    (defun next (m) (+ m 2))
    (* (/ h 3.0)
       (+ (funcall f a)
          (funcall f b)
          (* (sum #'fun-iter 1 #'next (- n 1)) 4)
          (* (sum #'fun-iter 2 #'next (- n 2)) 2)))))

;; The integral computation procedure above
(defun integral (f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(defun main ()
  (format t "Load this file and use Simpson formula. ~%")
  (format t "We use `cube` function for test. ~%")
  (format t "[Original Integral] [dx = 0.01 ] ~a ~%"
          (integral #'cube 0 1 0.01))
  (format t "[Simpson  Integral] [ n  = 100 ] ~a ~%"
          (simpson #'cube 0 1 100))
  (format t "[Original Integral] [dx = 0.001] ~a ~%"
          (integral #'cube 0 1 0.001))
  (format t "[Simpson  Integral] [ n  = 1000] ~a ~%"
          (simpson #'cube 0 1 1000)))

(main)


