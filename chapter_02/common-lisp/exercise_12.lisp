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
;;
;; Honestly, it is quite complicated to write such a function to
;; implement the multiplication of two intervals with center-percent
;; expression correctly. If the percentage of error is small, certainly
;; we can write a simple formula to compute the result of multiplication
;; under the assumption that the two centers of the given intervals are
;; both positive. We will show this in exercise 2.13. However, here we
;; still hold on challenging this problem: how to write a general and
;; correct formula to compute the multiplication?
;;
;; Here we give an available method to classify the problem into three
;; simple cases.
;;
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

;;
;; Test the following multiplication of intervals
;; [SBCL]
;;
;; CL-USER> (mul-interval-test)
;; PRINT INTERVAL: [10.000501, 29.999252] 
;; PRINT INTERVAL: 19.999876 (± 49.99719%) 
;; PRINT INTERVAL: [-15.0, 30.0] 
;; PRINT INTERVAL: 7.5 (± 300%) 
;; PRINT INTERVAL: [-30.0003, -1.9997998] 
;; PRINT INTERVAL: -16.00005 (± 87.50129%) 
;; PRINT INTERVAL: [-19.9995, 29.99925] 
;; PRINT INTERVAL: 4.999875 (± 500%) 
;; PRINT INTERVAL: [-20.0, 30.0] 
;; PRINT INTERVAL: 5.0 (± 500%) 
;; PRINT INTERVAL: [-21.000149, 30.000149] 
;; PRINT INTERVAL: 4.5 (± 566.67%) 
;; PRINT INTERVAL: [-20.000248, 10.000124] 
;; PRINT INTERVAL: -5.000062 (± 300%) 
;; PRINT INTERVAL: [-25.0, 50.0] 
;; PRINT INTERVAL: 12.5 (± 300%) 
;; PRINT INTERVAL: [-9.0, 6.0] 
;; PRINT INTERVAL: -1.5 (± 500%) 
;; PRINT INTERVAL: [-5.9998503, -2.0001001] 
;; PRINT INTERVAL: -3.9999752 (± 49.99719%) 
;; PRINT INTERVAL: [-6.0, 3.0] 
;; PRINT INTERVAL: -1.5 (± 300%) 
;; PRINT INTERVAL: [1.9999999, 12.0] 
;; PRINT INTERVAL: 7.0 (± 71.42857%) 
;; NIL
;;

(defun mul-interval-test ()
  (test-mul 2.5 20 7.5 33.33)
  (test-mul 2.5 20 2.5 300)
  (test-mul 2.5 20 -5.5 81.82)
  (test-mul 0.5 500 7.5 33.33)
  (test-mul 0.5 500 2.5 300)
  (test-mul 0.5 500 1.5 566.67)
  (test-mul -0.75 166.67 2.5 300)
  (test-mul -2.5 300 -2 150)
  (test-mul 0.5 500 -2 50)
  (test-mul -2.5 20 1.5 33.33)
  (test-mul -2.5 20 0.5 300)
  (test-mul -2.5 20 -2.5 60))

(defun main ()
  (format t "Load this file and use `make-interval`.~%")
  (format t "Here we test some intervals.~%")
  (format t "Use `(mul-interval-test)`.~%"))

(main)


