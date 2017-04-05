;; After debugging her program, Alyssa shows it to a potential user, who
;; complains that her program solves the wrong problem. He wants a program
;; that can deal with numbers represented as a center value and an
;; additive tolerance; for example, he wants to work with intervals such
;; as `3.5 ± 0.15` rather than [3.35, 3.65]. Alyssa returns to her desk
;; and fixes this problem by supplying an alternate constructor and
;; alternate selectors:
;;
;;     (define (make-center-width c w)
;;       (make-interval (- c w) (+ c w)))
;;     (define (center i)
;;       (/ (+ (lower-bound i) (upper-bound i)) 2))
;;     (define (width i)
;;       (/ (- (upper-bound i) (lower-bound i)) 2))
;;
;; Unfortunately, most of Alyssa's users are engineers. Real engineering
;; situations usually involve measurements with only a small uncertainty,
;; measured as the ratio of the width of the interval to the midpoint of
;; the interval. Engineers usually specify percentage tolerances on the
;; parameters of devices, as in the resistor specifications given earlier.
;;
;; The solution of exercise 2.12
;; Define a constructor make-center-percent that takes a center and a
;; percentage tolerance and produces the desired interval. You must also
;; define a selector percent that produces the percentage tolerance for a
;; given interval. The center selector is the same as the one shown above.
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
    :initform 0
    :reader center
    :documentation "The center of the interval")
   (percent
    :initarg :percent
    :initform 0
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
           (setf (slot-value itv 'upper-bound) add-bound-buf))
         (progn
           (setf (slot-value itv 'upper-bound) sub-bound-buf)
           (setf (slot-value itv 'lower-bound) add-bound-buf)))))

;; Extend `setf` function for this class
(defgeneric (setf percent) (new-percent itv))
(defgeneric (setf center) (new-center itv))
(defmethod (setf percent) (new-percent (itv interval))
  (if (< new-percent 0)
      (error "The percent must be positive!")
      (progn
        (with-accessors ((center center)) itv
          (setf (slot-value itv 'percent) new-percent)
          (slot-buf-reset new-percent center)
          (setf (slot-value itv 'width)
                (* 0.02 new-percent (abs center)))))))

(defmethod (setf center) (new-center (itv interval))
  (with-accessors ((percent percent)) itv
    (setf (slot-value itv 'center) new-center)
    (slot-buf-reset percent new-center)
    (setf (slot-value itv 'width)
          (* 0.02 percent (abs new-center)))))

;; Define an :after method specialized on `inter` class to check whether
;; the :percent is negative.
(defmethod initialize-instance :after ((itv interval) &key)
  (with-accessors ((percent percent)
                   (center center)) itv
    (if (> 0 percent)
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

;;
;; The arithmetic operations will be defined later.
;; This exercise is just for testing.
;;

(defun main ()
  (format t "Load this file and use `make-interval`.~%"))

(main)


