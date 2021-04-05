;; The solution of exercise 2.11
;; In passing, Ben also cryptically comments: "By testing the signs of
;; the endpoints of the intervals, it is possible to break `mul-interval`
;; into nine cases, only one of which requires more than two
;; multiplications." Rewrite this procedure using Ben's suggestion.
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

;;
;; To rewrite the function `mul-interval` using Ben's suggestion, we have
;; to consider 9 complicated cases listed following.
;;
;;     lb-x (a)  ub-x (b)  lb-y (c)  ub-y (d)   lb-result   ub-result
;; [1]    P         P         P         P         a * c       b * d
;; [2]    P         P         N         P         b * c       b * d
;; [3]    P         P         N         N         b * c       a * d
;; [4]    N         P         P         P         a * d       b * d
;;                                                | a * d      | a * c
;; [5]    N         P         N         P     min<         max<
;;                                                | b * c      | b * d
;; [6]    N         P         N         N         b * c       a * c
;; [7]    N         N         P         P         a * d       b * c
;; [8]    N         N         N         P         a * d       a * c
;; [9]    N         N         N         N         b * d       a * c
;;
;; where `P` means `non-negative` instead of `positive`, while `N` means
;; `negative`. Only case 5 requires more than two multiplications.
;;

(defgeneric mul-interval (itv-x itv-y))
(defmethod mul-interval ((itv-x interval) (itv-y interval))
  (with-accessors ((lb-x lower-bound)
                   (ub-x upper-bound)) itv-x
    (with-accessors ((lb-y lower-bound)
                     (ub-y upper-bound)) itv-y
      (cond ((>= lb-x 0)
             (cond ((>= lb-y 0)
                    (make-interval (* lb-x lb-y)
                                   (* ub-x ub-y)))
                   ((< ub-y 0)
                    (make-interval (* ub-x lb-y)
                                   (* lb-x ub-y)))
                   (t
                    (make-interval (* ub-x lb-y)
                                   (* ub-x ub-y)))))
            ((< ub-x 0)
             (cond ((>= lb-y 0)
                    (make-interval (* lb-x ub-y)
                                   (* ub-x lb-y)))
                   ((< ub-y 0)
                    (make-interval (* ub-x ub-y)
                                   (* lb-x lb-y)))
                   (t
                    (make-interval (* lb-x ub-y)
                                   (* lb-x lb-y)))))
            (t
             (cond ((>= lb-y 0)
                    (make-interval (* lb-x ub-y)
                                   (* ub-x ub-y)))
                   ((< ub-y 0)
                    (make-interval (* ub-x lb-y)
                                   (* lb-x lb-y)))
                   (t
                    (make-interval (min (* ub-x lb-y)
                                        (* lb-x ub-y))
                                   (max (* lb-x lb-y)
                                        (* ub-x ub-y))))))))))

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

;; Define a macro for testing
(defmacro test-mul (lb-x ub-x lb-y ub-y)
  `(print-interval
    (mul-interval
     (make-interval ,lb-x ,ub-x)
     (make-interval ,lb-y ,ub-y))))

;;
;; JUST A TEST:
;;
;; [SBCL]
;;     CL-USER> (test-mul 2 3 5 10)
;;     PRINT INTERVAL: [10, 30]
;;
;;     CL-USER> (test-mul 2 3 -5 10)
;;     PRINT INTERVAL: [-15, 30] 
;;
;;     CL-USER> (test-mul 2 3 -10 -1)
;;     PRINT INTERVAL: [-30, -2] 
;;
;;     CL-USER> (test-mul -2 3 5 10)
;;     PRINT INTERVAL: [-20, 30] 
;;
;;     CL-USER> (test-mul -2 3 -5 10)
;;     PRINT INTERVAL: [-20, 30] 
;;
;;     CL-USER> (test-mul -2 3 -7 10)
;;     PRINT INTERVAL: [-21, 30] 
;;
;;     CL-USER> (test-mul -2 0.5 -5 10)
;;     PRINT INTERVAL: [-20, 10] 
;;
;;     CL-USER> (test-mul -10 5 -5 1)
;;     PRINT INTERVAL: [-25, 50] 
;;
;;     CL-USER> (test-mul -2 3 -3 -1)
;;     PRINT INTERVAL: [-9, 6] 
;;
;;     CL-USER> (test-mul -3 -2 1 2)
;;     PRINT INTERVAL: [-6, -2] 
;;
;;     CL-USER> (test-mul -3 -2 -1 2)
;;     PRINT INTERVAL: [-6, 3] 
;;
;;     CL-USER> (test-mul -3 -2 -4 -1)
;;     PRINT INTERVAL: [2, 12] 
;;
;;

(defun main ()
  (format t "Load this file and use `make-interval`.~%"))

(main)


