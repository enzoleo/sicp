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
;; way to print points:
;;
;;     (define (print-point p)
;;       (display "(")
;;       (display (x-point p))
;;       (display ",")
;;       (display (y-point p))
;;       (display ")")
;;       (newline))
;;
;; -------- (above from SICP)
;;

;; Make points
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

;; Make segments
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

;; Compute the coordinates of the midpoint of the given segment
(define (midpoint-segment segment)
  (let* ((start-point (start-segment segment))
         (end-point (end-segment segment))
         (start-x-point (x-point start-point))
         (start-y-point (y-point start-point))
         (end-x-point (x-point end-point))
         (end-y-point (y-point end-point)))
    (make-point (/ (+ start-x-point
                      end-x-point)
                   2.0)
                (/ (+ start-y-point
                      end-y-point)
                   2.0))))

;; Print the coordinate of one point
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define (main)
  (display "Load this file and use `midpoint-segment` procedure.\n")
  (display "Example: A(1.5, 7.5) . B(-1.5, -8.5)\n")
  (display "Make segment and print its midpoint: \n")
  (let ((test-segment
         (make-segment (make-point 1.5 7.5)
                       (make-point -1.5 -8.5))))
    (print-point (midpoint-segment test-segment))))

(main)


