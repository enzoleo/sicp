;; The solution of exercise 2.10
;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's
;; shoulder and comments that it is not clear what it means to divide by
;; an interval that spans zero. Modify Alyssa's code to check for this
;; condition and to signal an error if it occurs.
;;
;; -------- (above from SICP)
;;

;; Define the interval class
(defclass interval ()
  ((lower-bound
    :initarg :lower-bound
    :initform 0
    :reader lower-bound
    :documentation "The lower bound")
   (upper-bound
    :initarg :upper-bound
    :initform 1
    :reader upper-bound
    :documentation "The upper bound")))

;; Extend `setf` function for this class
(defgeneric (setf upper-bound) (ub itv))
(defmethod (setf upper-bound) (ub (itv interval))
  (if (> (lower-bound itv) ub)
      (error "The lower bound is larger than the upper bound!")
      (setf (slot-value itv 'upper-bound) ub)))

(defgeneric (setf lower-bound) (lb itv))
(defmethod (setf lower-bound) (lb (itv interval))
  (if (< (upper-bound itv) lb)
      (error "The lower bound is larger than the upper bound!")
      (setf (slot-value itv 'lower-bound) lb)))

;; Define an :after method specialized on `inter` class to check whether
;; the :lower-bound is larger than the :upper-bound
(defmethod initialize-instance :after ((itv interval) &key)
  (with-accessors ((lb lower-bound)
                   (ub upper-bound)) itv
    (if (> lb ub)
        (error "Illegal construction of class interval."))))

;; Define a macro for construction
(defmacro make-interval (lb ub)
  `(make-instance 'interval
                  :lower-bound ,lb
                  :upper-bound ,ub))

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

(defgeneric sub-interval (itv-x itv-y))
(defmethod sub-interval ((itv-x interval) (itv-y interval))
  (make-instance 'interval
                 :lower-bound (- (lower-bound itv-x)
                                 (upper-bound itv-y))
                 :upper-bound (- (upper-bound itv-x)
                                 (lower-bound itv-y))))

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
  (with-accessors ((lb-y lower-bound)
                   (ub-y upper-bound)) itv-y
    (if (or (< ub-y 0) (> lb-y 0))
        (mul-interval
         itv-x
         (make-instance 'interval
                        :lower-bound (/ 1.0 (upper-bound itv-y))
                        :upper-bound (/ 1.0 (lower-bound itv-y))))
        (error "The dividend interval spans zero!"))))

;;
;; JUST A TEST:
;;
;; [SBCL]
;;     (defparameter ix (make-interval 1 5))
;;     (defparameter iy (make-interval -1 4))
;;
;; CL-USER> (print-interval (add-interval ix iy))
;; PRINT INTERVAL: [0, 9] 
;;
;; CL-USER> (print-interval (sub-interval ix iy))
;; PRINT INTERVAL: [-3, 6]
;;
;; CL-USER> (print-interval (mul-interval ix iy))
;; PRINT INTERVAL: [-5, 20] 
;; 
;; CL-USER> (print-interval (div-interval iy ix))
;; PRINT INTERVAL: [-1.0, 4.0]
;;
;; CL-USER> (print-interval (div-interval ix iy))
;; The dividend interval spans zero!
;; [Condition of type SIMPLE-ERROR]
;;

(defun main ()
  (format t "Load this file and use `make-interval`.~%"))

(main)


