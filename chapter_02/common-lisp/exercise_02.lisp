;; The solution of exercise 2.2
;; Consider the problem of representing line segments in a plane. Each
;; segment is represented as a pair of points: a starting point and an
;; ending point. Define a constructor `make-segment` and selectors `start-
;; segment` and `end-segment` that define the representation of segments
;; in terms of points. Furthermore, a point can be represented as a pair
;; of numbers: the x coordinate and the y coordinate. Accordingly, specify
;; a constructor make-point and selectors `x-point` and `y-point` that
;; define this representation.
;;
;; Finally, using your selectors and constructors, define a procedure
;; `midpoint-segment` that takes a line segment as argument and returns
;; its midpoint (the point whose coordinates are the average of the
;; coordinates of the endpoints). To try your procedures, you'll need a
;; way to print points.
;;
;; -------- (above from SICP)
;;

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

;; Define generic functions and new methods
(defgeneric print-point (point))
(defgeneric print-segment (seg))
(defmethod print-point ((point point-2d))
  (format t "PRINT POINT: (~a, ~a) ~%" (x point) (y point)))
(defmethod print-segment ((seg segment))
  (let ((start (start seg))
        (end (end seg)))
    (format t "START POINT: (~a, ~a)~%" (x start) (y start))
    (format t "END   POINT: (~a, ~a)~%" (x end) (y end))))

;; Compute the coordinates or midpoint of the given segment
(defgeneric midpoint-segment (seg))
(defmethod midpoint-segment ((seg segment))
  (let* ((start (start seg))
         (end (end seg))
         (mid-x (/ (+ (x start)
                      (x end))
                   2.0))
         (mid-y (/ (+ (y start)
                      (y end))
                   2.0)))
    (make-instance 'point-2d
                   :x mid-x
                   :y mid-y)))

  
