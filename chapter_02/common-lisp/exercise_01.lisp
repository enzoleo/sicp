;; The solution of exercise 2.1
;; Define a better version of make-rat that handles both positive and
;; negative arguments. Make-rat should normalize the sign so that if the
;; rational number is positive, both the numerator and denominator are
;; positive, and if the rational number is negative, only the numerator is
;; negative.
;;
;; -------- (above from SICP)
;;

;; Define a new rational class as we already have a rational class by
;; default. (Here we set the init value equal to 0 / 1)
(defclass my-rational ()
  ((numer
    :initarg :numer
    :initform 0
    :accessor numer
    :documentation "The numerator")
   (denom
    :initarg :denom
    :initform 1
    :reader denom
    :documentation "The denominator")))

;; Extend `setf` function for this class
(defgeneric (setf denom) (new-denom rat))
(defmethod (setf denom) (new-denom (rat my-rational))
  (if (= new-denom 0)
      (error "The denominator cannot be zero!")
      (setf (slot-value rat 'denom) new-denom)))

;; Define an :after method specialized on `my-rational` class to add
;; custom initialization code.
(defmethod initialize-instance :after ((rat my-rational) &key)
  (with-accessors ((tmp-numer numer)
                   (tmp-denom denom)) rat
    (if (= tmp-denom 0)
        (error "The denominator cannot be zero!")
        (let ((gcdiv (gcd tmp-numer tmp-denom)))
          (setf tmp-numer (/ tmp-numer gcdiv))
          (setf tmp-denom (/ tmp-denom gcdiv))
          (when (< tmp-denom 0)
            (setf (denom rat) (- tmp-denom))
            (setf (numer rat) (- tmp-numer)))))))

;; Define a macro for construction
(defmacro make-my-rat (new-numer new-denom)
  `(make-instance 'my-rational
                  :numer ,new-numer
                  :denom ,new-denom))

;; Define generic functions
(defgeneric add-rat (rat-x rat-y))
(defgeneric sub-rat (rat-x rat-y))
(defgeneric mul-rat (rat-x rat-y))
(defgeneric div-rat (rat-x rat-y))
(defgeneric equal-rat? (rat-x rat-y))

;; Define methods
(defmethod add-rat ((rat-x my-rational)
                    (rat-y my-rational))
  (make-instance 'my-rational
                 :numer (+ (* (numer rat-x) (denom rat-y))
                           (* (numer rat-y) (denom rat-x)))
                 :denom (* (denom rat-x) (denom rat-y))))

(defmethod sub-rat ((rat-x my-rational)
                    (rat-y my-rational))
  (make-instance 'my-rational
                 :numer (- (* (numer rat-x) (denom rat-y))
                           (* (numer rat-y) (denom rat-x)))
                 :denom (* (denom rat-x) (denom rat-y))))

(defmethod mul-rat ((rat-x my-rational)
                    (rat-y my-rational))
  (make-instance 'my-rational
                 :numer (* (numer rat-x) (numer rat-y))
                 :denom (* (denom rat-x) (denom rat-y))))

(defmethod div-rat ((rat-x my-rational)
                    (rat-y my-rational))
  (make-instance 'my-rational
                 :numer (* (numer rat-x) (denom rat-y))
                 :denom (* (denom rat-x) (numer rat-y))))

(defmethod equal-rat? ((rat-x my-rational)
                       (rat-y my-rational))
  (= (* (numer rat-x) (denom rat-y))
     (* (numer rat-y) (denom rat-x))))

(defgeneric print-my-rat (rat))
(defmethod print-my-rat ((rat my-rational))
  (format t "PRINT RATIONAL: ~a / ~a ~%"
          (numer rat)
          (denom rat)))

;;
;; Load this file and use the new defined class `my-rational`. This class
;; is only used for testing.
;;
;; Initialize:
;;     (defparameter my-rat
;;                   (make-instance 'my-rational
;;                                  :numer <init numerator>
;;                                  :denom <init denominator>))
;;
;; Or:
;;     (defparameter my-rat (make-my-rat <init numerator>
;;                                       <init denominator>))
;;
;; Accessors:
;;     (numer my-rat)   -> init numerator
;;     (denom my-rat)   -> init denominator
;;
;;


(defun main ()
  (format t "Load this file and use `make-my-rational`.~%"))

(main)


