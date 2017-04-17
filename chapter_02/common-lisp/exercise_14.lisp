;; After considerable work, Alyssa P. Hacker delivers her finished system.
;; Several years later, after she has forgotten all about it, she gets a
;; frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has
;; noticed that the formula for parallel resistors can be written in two
;; algebraically equivalent ways:
;;
;;      R_1 * R_2                         1
;;     -----------      and      -------------------
;;      R_1 + R_2                 1 / R_1 + 1 / R_2
;;
;; He has written the following two programs, each of which computes the
;; parallel-resistors formula differently:
;;
;;     (define (par1 r1 r2)
;;         (div-interval (mul-interval r1 r2)
;;                       (add-interval r1 r2)))
;;     (define (par2 r1 r2)
;;         (let ((one (make-interval 1 1)))
;;           (div-interval one
;;                         (add-interval (div-interval one r1)
;;                                       (div-interval one r2)))))
;;
;; Lem complains that Alyssa's program gives different answers for the two
;; ways of computing. This is a serious complaint.
;;
;; The solution of exercise 2.14
;; Demonstrate that Lem is right. Investigate the behavior of the system
;; on a variety of arithmetic expressions. Make some intervals A and B,
;; and use them in computing the expressions A / A and A / B. You will get
;; the most insight by using intervals whose width is a small percentage of
;; the center value. Examine the results of the computation in center-
;; percent form (see exercise 2.12).
;;
;; -------- (above from SICP)
;;

;;
;; Define the interval class.
;; Here we define a interval class with 5 slots: center, percent, width,
;; lower-bound and upper-bound, but only @center and @percent is allowed
;; to be modified (or reset) to update the new interval, which is a little
;; different from that in exercise 2.11.
;;
;; To construct the instance of this class, only 2 arguments (@center and
;; @percent) is needed, and the contructor will automatically compute
;; the other slot-values.
;;
(defclass interval ()
  ((center
    :initarg :center
    :initform (error "Must supply the center.")
    :reader center
    :documentation "The center of the interval")
   (percent
    :initarg :percent
    :initform (error "Must supply the percent.")
    :reader percent
    :documentation "The percentage tolerance")
   (width
    :initarg :width
    :initform 0
    :reader width
    :documentation "The width of the interval")
   (lower-bound
    :initarg :lower-bound
    :initform 0
    :reader lower-bound
    :documentation "The lower bound")
   (upper-bound
    :initarg :upper-bound
    :initform 0
    :reader upper-bound
    :documentation "The upper bound")))

;; Define a new macro to reset the slot values
(defmacro slot-buf-reset (buf-pct buf-ctr)
  `(let ((sub-bound-buf (* (/ (- 100 ,buf-pct) 100.0) ,buf-ctr))
         (add-bound-buf (* (/ (+ 100 ,buf-pct) 100.0) ,buf-ctr)))
     (if (plusp ,buf-ctr)
         (progn
           (setf (slot-value itv 'lower-bound) sub-bound-buf)
           (setf (slot-value itv 'upper-bound) add-bound-buf)
           (setf (slot-value itv 'width)
                 (/ (- add-bound-buf sub-bound-buf) 2)))
         (progn
           (setf (slot-value itv 'upper-bound) sub-bound-buf)
           (setf (slot-value itv 'lower-bound) add-bound-buf)
           (setf (slot-value itv 'width)
                 (/ (- sub-bound-buf add-bound-buf) 2))))))

;; Extend `setf` function for this class
(defgeneric (setf percent) (new-percent itv))
(defgeneric (setf center) (new-center itv))
(defmethod (setf percent) (new-percent (itv interval))
  (if (< new-percent 0)
      (error "The percent must be positive!")
      (progn
        (with-accessors ((center center)) itv
          (setf (slot-value itv 'percent) new-percent)
          (slot-buf-reset new-percent center)))))

(defmethod (setf center) (new-center (itv interval))
  (if (zerop new-center)
      (error "Illegal center value!")
      (with-accessors ((percent percent)) itv
        (setf (slot-value itv 'center) new-center)
        (slot-buf-reset percent new-center))))

;; Define an :after method specialized on `inter` class to check whether
;; the :percent is negative.
(defmethod initialize-instance :after ((itv interval) &key)
  (with-accessors ((percent percent)
                   (center center)) itv
    (if (or (> 0 percent)
            (zerop center))
        (error "Illegal construction of class interval.")
        (slot-buf-reset percent center))))

;; Define a macro for construction
(defmacro make-interval (center percent)
  `(make-instance 'interval
                  :center ,center
                  :percent ,percent))

;; Define generic functions and new methods
;; Here we define two functions to print intervals
(defgeneric print-real-interval (itv))
(defgeneric print-interval (itv))
(defmethod print-real-interval ((itv interval))
  (format t "PRINT INTERVAL: [~a, ~a] ~%"
          (lower-bound itv)
          (upper-bound itv)))
(defmethod print-interval ((itv interval))
  (format t "PRINT INTERVAL: ~a (± ~a%) ~%"
          (center itv)
          (percent itv)))

;; The definitions of arithmetic operations
(defgeneric add-interval (itv-x itv-y))
(defmethod add-interval ((itv-x interval) (itv-y interval))
  (with-slots ((center-ix center)
               (percent-ix percent)) itv-x
    (with-slots ((center-iy center)
                 (percent-iy percent)) itv-y
      (if (zerop (+ center-ix center-iy))
          (error "Illegal interval center (zero).")
          (make-instance 'interval
                         :center (+ center-ix center-iy)
                         :percent (/ (+ (* percent-ix (abs center-ix))
                                        (* percent-iy (abs center-iy)))
                                     (* (abs (+ center-ix center-iy))
                                        1.0)))))))

(defgeneric sub-interval (itv-x itv-y))
(defmethod sub-interval ((itv-x interval) (itv-y interval))
  (with-slots ((center-ix center)
               (percent-ix percent)) itv-x
    (with-slots ((center-iy center)
                 (percent-iy percent)) itv-y
      (if (= center-ix center-iy)
          (error "Illegal interval center (zero).")
          (make-instance 'interval
                         :center (- center-ix center-iy)
                         :percent (/ (+ (* percent-ix (abs center-ix))
                                        (* percent-iy (abs center-iy)))
                                     (* (abs (- center-ix center-iy))
                                        1.0)))))))

(defgeneric mul-interval (itv-x itv-y))
(defmethod mul-interval ((itv-x interval) (itv-y interval))
  (with-slots ((ctr-ix center)
               (pct-ix percent)) itv-x
    (with-slots ((ctr-iy center)
                 (pct-iy percent)) itv-y
      (cond ((or (and (>= pct-iy 100) (< pct-ix 100))
                 (and (>= pct-ix 100) (> pct-iy pct-ix)))
             (make-interval (* (+ 1 (/ pct-ix 100.0))
                               ctr-ix ctr-iy)
                            pct-iy))
            ((and (< pct-ix 100)
                  (< pct-iy 100))
             (make-interval (* (+ 1 (/ (* pct-ix pct-iy)
                                       10000.0))
                               ctr-ix ctr-iy)
                            (/ (* (+ pct-ix pct-iy)
                                  10000.0)
                               (+ (* pct-ix
                                     pct-iy) 10000))))
            (t
             (make-interval (* (+ 1 (/ pct-iy 100.0))
                               ctr-ix ctr-iy)
                            pct-ix))))))

(defgeneric div-interval (itv-x itv-y))
(defmethod div-interval ((itv-x interval) (itv-y interval))
  (with-accessors ((ctr-y center)
                   (pct-y percent)) itv-y
    (if (< pct-y 100)
        (mul-interval
         itv-x
         (make-instance 'interval
                        :center  (/ (/ 10000.0 ctr-y)
                                    (- 10000 (* pct-y pct-y)))
                        :percent pct-y))
        (error "The dividend interval spans zero!"))))

;; Define a macro for testing
(defmacro test-mul (ctr-x pct-x ctr-y pct-y)
  `(let ((tmp-mul
          (mul-interval (make-interval ,ctr-x ,pct-x)
                        (make-interval ,ctr-y ,pct-y))))
     (print-real-interval tmp-mul)
     (print-interval tmp-mul)))

(defgeneric par1 (r1 r2))
(defmethod par1 ((r1 interval) (r2 interval))
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defgeneric par2 (r1 r2))
(defmethod par2 ((r1 interval) (r2 interval))
  (let ((one (make-interval 1 0)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;
;; [SBCL]
;;
;; CL-USER> (print-interval (div-interval ix ix))
;; PRINT INTERVAL: 1.0002 (± 1.9998%) 
;;
;; CL-USER> (print-interval (div-interval ix iy))
;; PRINT INTERVAL: 0.33353344 (± 2.9994001%)
;;
;; CL-USER> (defparameter iz1 (par1 ix iy))
;; IZ1
;; CL-USER> (defparameter iz2 (par2 ix iy))
;; IZ2
;; CL-USER> (print-interval iz1)
;; PRINT INTERVAL: 7.507737 (± 4.746908%)
;;
;; CL-USER> (print-interval iz2)
;; PRINT INTERVAL: 7.4998593 (± 1.2500563%)
;;

(defun main ()
  (format t "Load this file and use `par1` and `par2`.~%")
  (format t "Let ix = 10 (± 1%) and iy = 30 (± 2%).~%")
  (format t "Test (div-interval ix ix) and (div-interval ix iy).~%"))

(main)


