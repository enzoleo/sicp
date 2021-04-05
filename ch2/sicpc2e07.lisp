;; The solution of exercise 2.7
;; Alyssa's program is incomplete because she has not specified the
;; implementation of the interval abstraction. Here is a definition of the
;; interval constructor:
;;
;;     (define (make-interval a b) (cons a b))
;;
;; Define selectors `upper-bound` and `lower-bound` to complete the
;; implementation.
;;
;; -------- (above from SICP)
;;

;; Define the interval class
(defclass interval ()
  ((lower-bound
    :initarg :lower-bound
    :initform 0
    :accessor lower-bound
    :documentation "The lower bound")
   (upper-bound
    :initarg :upper-bound
    :initform 1
    :accessor upper-bound
    :documentation "The upper bound")))

;; Define generic functions and new methods
(defgeneric print-interval (itv))
(defmethod print-interval ((itv interval))
  (format t "PRINT INTERVAL: [~a, ~a] ~%"
          (lower-bound itv)
          (upper-bound itv)))

(defgeneric add-interval (itv-x itv-y))
(defmethod add-interval ((itv-x interval) (itv-y interval))
  (make-instance 'interval
                 :lower-bound (+ (lower-bound itv-x)
                                 (lower-bound itv-y))
                 :upper-bound (+ (upper-bound itv-x)
                                 (upper-bound itv-y))))

(defgeneric mul-interval (itv-x itv-y))
(defmethod mul-interval ((itv-x interval) (itv-y interval))
  (let ((p1 (* (lower-bound itv-x) (lower-bound itv-y)))
        (p2 (* (lower-bound itv-x) (upper-bound itv-y)))
        (p3 (* (upper-bound itv-x) (lower-bound itv-y)))
        (p4 (* (upper-bound itv-x) (upper-bound itv-y))))
    (make-instance 'interval
                   :lower-bound (min p1 p2 p3 p4)
                   :upper-bound (max p1 p2 p3 p4))))

(defgeneric div-interval (itv-x itv-y))
(defmethod div-interval ((itv-x interval) (itv-y interval))
  (mul-interval itv-x
                (make-instance 'interval
                               :lower-bound (/ 1.0 (upper-bound itv-y))
                               :upper-bound (/ 1.0 (lower-bound itv-y)))))

;;
;; JUST A TEST:
;;
;; [SBCL]
;;     (defparameter ix
;;       (make-instance 'interval
;;                      :lower-bound 1
;;                      :upper-bound 5))
;;
;;     (defparameter iy
;;       (make-instance 'interval
;;                      :lower-bound -1
;;                      :upper-bound 4))
;;
;; CL-USER> (print-interval (add-interval ix iy))
;; PRINT INTERVAL: [0, 9] 
;; 
;; CL-USER> (print-interval (mul-interval ix iy))
;; PRINT INTERVAL: [-5, 20] 
;; 
;; CL-USER> (print-interval (div-interval ix iy))
;; PRINT INTERVAL: [-5.0, 1.25]
;;

(defun main ()
  (format t "Load this file and use `make-interval`.~%"))

(main)


