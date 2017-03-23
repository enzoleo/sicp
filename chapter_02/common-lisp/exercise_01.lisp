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
    :initform 0)
   (denom
    :initarg :denom
    :initform 1)))

;; Define an :after method specialized on `my-rational` class to add
;; custom initialization code.
(defmethod initialize-instance :after ((rat my-rational) &key)
  (let* ((numer (slot-value rat 'numer))
         (denom (slot-value rat 'denom))
         (gcdiv (gcd numer denom)))
    (setf numer (/ numer gcdiv))
    (setf denom (/ denom gcdiv))
    (when (< denom 0)
      (setf denom (- denom))
      (setf numer (- numer)))
    (setf (slot-value rat 'numer) numer)
    (setf (slot-value rat 'denom) denom)))

;; Define generic functions
(defgeneric numer (rat))
(defgeneric denom (rat))
(defgeneric add-rat (rat-x rat-y))
(defgeneric sub-rat (rat-x rat-y))
(defgeneric mul-rat (rat-x rat-y))
(defgeneric div-rat (rat-x rat-y))
(defgeneric equal-rat? (rat-x rat-y))

;; Define methods
(defmethod numer ((rat my-rational))
  (slot-value rat 'numer))
(defmethod denom ((rat my-rational))
  (slot-value rat 'denom))

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
;; Accessors:
;;     (numer my-rat)   -> init numerator
;;     (denom my-rat)   -> init denominator
;;
;;


