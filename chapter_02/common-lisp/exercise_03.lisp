;; The solution of exercise 2.3
;; Implement a representation for rectangles in a plane. (Hint: You may
;; want to make use of exercise 2.2.) In terms of your constructors and
;; selectors, create procedures that compute the perimeter and the area of
;; a given rectangle. Now implement a different representation for
;; rectangles. Can you design your system with suitable abstraction
;; barriers, so that the same perimeter and area procedures will work
;; using either representation? 
;;
;; -------- (above from SICP)
;;

;; Compute the square of a number
(defun square (x) (* x x))

;; Define a new 2D points class over R x R set (Euclidean plane).
;; Here we set the init value: ordinate origin (0, 0).
(defclass point-2d ()
  ((x :initarg :x
      :initform 0
      :accessor x
      :documentation "The x-coordinate")
   (y :initarg :y
      :initform 0
      :accessor y
      :documentation "The y-coordinate")))

;; Define a new simple 2D vector class
(defclass vec-2d ()
  ((x :initarg :x
      :initform 0
      :accessor x
      :documentation "The x-coordinate")
   (y :initarg :y
      :initform 0
      :accessor y
      :documentation "The y-coordinate")))

;; Define segment class
(defclass segment ()
  ((start
    :initarg :start
    :initform (make-instance 'point-2d)
    :accessor start
    :documentation "The start point")
   (end
    :initarg :end
    :initform (make-instance 'point-2d)
    :accessor end
    :documentation "The end point")))

;; Define parallelogram class
(defclass parallelogram ()
  ((basepoint
    :initarg :basepoint
    :initform (make-instance 'point-2d)
    :accessor basepoint
    :documentation "The base point")
   (edge-va
    :initarg :edge-va
    :initform (make-instance 'vec-2d)
    :accessor edge-va
    :documentation "Edge vector")
   (edge-vb
    :initarg :edge-vb
    :initform (make-instance 'vec-2d)
    :accessor edge-vb
    :documentation "Edge vector")))

;; Define generic functions and new methods
(defgeneric print-point (point))
(defgeneric print-vec (vec))
(defgeneric print-segment (seg))
(defmethod print-point ((point point-2d))
  (format t "PRINT POINT: (~a, ~a) ~%" (x point) (y point)))
(defmethod print-vec ((vec vec-2d))
  (format t "PRINT VEC: (~a, ~a) ~%" (x vec) (y vec)))
(defmethod print-segment ((seg segment))
  (with-accessors ((start start)
                   (end end)) seg
    (format t "START POINT: (~a, ~a)~%" (x start) (y start))
    (format t "END   POINT: (~a, ~a)~%" (x end) (y end))))

;; Define Euclidean distance of 2 points pa and pb
(defgeneric euclidean-dist (pa pb))
(defmethod euclidean-dist ((pa point-2d) (pb point-2d))
  (with-accessors ((pa-x x) (pa-y y)) pa
    (with-accessors ((pb-x x) (pb-y y)) pb
      (sqrt (+ (square (- pb-x pa-x))
               (square (- pb-y pa-y)))))))

;; The basic operation on vectors and points
;; [1] Plus two vectors and return the sum vector
;; [2] Translate a point with a vector
(defgeneric plus-vec (va vb))
(defmethod plus-vec ((va vec-2d) (vb vec-2d))
  (make-instance 'vec-2d
                 :x (+ (x va) (x vb))
                 :y (+ (y va) (y vb))))
(defmethod plus-vec ((point point-2d) (vec vec-2d))
  (make-instance 'point-2d
                 :x (+ (x point) (x vec))
                 :y (+ (y point) (y vec))))

;; The dot product of two vectors
(defgeneric dot-product (va vb))
(defmethod dot-product ((va vec-2d) (vb vec-2d))
  (+ (* (x va) (x vb))
     (* (y va) (y vb))))

;; Compute the length of a vector
(defgeneric get-length (vec))
(defmethod get-length ((vec vec-2d))
  (euclidean-dist (start vec) (end vec)))

;; Compute the area of a parallelogram
(defgeneric get-area (paral))
(defmethod get-area ((paral parallelogram))
  (with-accessors ((edge-va edge-va)
                   (edge-vb edge-vb)) paral
    (abs (- (* (x edge-va) (y edge-vb))
            (* (y edge-va) (x edge-vb))))))



