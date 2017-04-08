;; The solution of exercise 2.13
;; Show that under the assumption of small percentage tolerances there is
;; a simple formula for the approximate percentage tolerance of the
;; product of two intervals in terms of the tolerances of the factors. You
;; may simplify the problem by assuming that all numbers are positive.
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

;; The new multiplication method is defined here, under the assumption
;; of small percentage tolerances .
(defgeneric mul-interval (itv-x itv-y))
(defmethod mul-interval ((itv-x interval) (itv-y interval))
  (with-slots ((ctr-ix center)
               (pct-ix percent)) itv-x
    (with-slots ((ctr-iy center)
                 (pct-iy percent)) itv-y
      (make-interval (* ctr-ix ctr-iy)
                     (+ pct-ix pct-iy)))))

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

;;
;; Test the following multiplication of intervals
;; [SBCL]
;;
;; CL-USER> (mul-interval-test)
;; PRINT INTERVAL: [1865.625, 1884.375] 
;; PRINT INTERVAL: 1875 (± 0.5%) 
;; PRINT INTERVAL: [621.875, 628.125] 
;; PRINT INTERVAL: 625 (± 0.5%) 
;; PRINT INTERVAL: [-1380.5001, -1369.5] 
;; PRINT INTERVAL: -1375 (± 0.4%) 
;; PRINT INTERVAL: [4092.0, 4158.0] 
;; PRINT INTERVAL: 4125 (± 0.8%) 
;; PRINT INTERVAL: [1364.0, 1386.0] 
;; PRINT INTERVAL: 1375 (± 0.8%) 
;; PRINT INTERVAL: [4628.25, 4721.75] 
;; PRINT INTERVAL: 4675 (± 1.0%) 
;; PRINT INTERVAL: [-1883.6251, -1866.375] 
;; PRINT INTERVAL: -1875 (± 0.46%) 
;; PRINT INTERVAL: [1493.25, 1506.7499] 
;; PRINT INTERVAL: 1500 (± 0.45000002%) 
;; PRINT INTERVAL: [-1666.5, -1633.5] 
;; PRINT INTERVAL: -1650 (± 1.0%) 
;; PRINT INTERVAL: [-1884.375, -1865.625] 
;; PRINT INTERVAL: -1875 (± 0.5%) 
;; PRINT INTERVAL: [-1381.875, -1368.125] 
;; PRINT INTERVAL: -1375 (± 0.5%) 
;; PRINT INTERVAL: [622.5, 627.50006] 
;; PRINT INTERVAL: 625 (± 0.4%) 
;; NIL
;;

(defun mul-interval-test ()
  (test-mul 25 0.2 75 0.3)
  (test-mul 25 0.2 25 0.3)
  (test-mul 25 0.2 -55 0.2)
  (test-mul 55 0.5 75 0.3)
  (test-mul 55 0.5 25 0.3)
  (test-mul 55 0.5 85 0.5)
  (test-mul -75 0.16 25 0.3)
  (test-mul -25 0.3 -60 0.15)
  (test-mul 55 0.5 -30 0.5)
  (test-mul -25 0.2 75 0.3)
  (test-mul -25 0.2 55 0.3)
  (test-mul -25 0.2 -25 0.2))

(defun main ()
  (format t "Load this file and use `make-interval`.~%")
  (format t "Here we test some intervals.~%")
  (format t "Use `(mul-interval-test)`.~%"))

(main)


