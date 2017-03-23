;; The solution of exercise 2.3
;; 
;;
;; -------- (above from SICP)
;;

;; Compute the square of a number
(define (square x) (* x x))

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

;; Define Euclidean distance of 2 points pa and pb
(define (euc-distance pa pb)
  (let ((pa-x (x-point pa))
        (pa-y (y-point pa))
        (pb-x (x-point pb))
        (pb-y (y-point pb)))
    (sqrt (+ (square (- pb-x pa-x))
             (square (- pb-y pa-y))))))

;; Make a rectangle in two ways
;;
;; ---- METHOD 01
(define (make-rectangle x-edge y-edge) (cons x-edge y-edge))

;; Return the two edges which are vertical to each other
(define (get-x-edge rec) (car rec))
(define (get-y-edge rec) (cdr rec))

;; Return the length and width
(define (get-length rec)
  (let ((x-edge (get-x-edge rec)))
    (euc-distance
     (start-segment x-edge)
     (end-segment x-edge))))

;; Return the length and width
(define (get-width rec)
  (let ((y-edge (get-y-edge rec)))
    (euc-distance
     (start-segment y-edge)
     (end-segment y-edge))))

;; Compute the perimeter
(define (get-perimeter rec)
  (* 2
     (+ (get-length rec)
        (get-width rec))))

;; Compute the area
(define (get-area rec)
  (* (get-length rec)
     (get-width rec)))

(define (main)
  (display "Load this file and use `midpoint-segment` procedure.\n")
  (display "Example: A(1.5, 7.5) . B(-1.5, -8.5)\n")
  (display "Make segment and print its midpoint: \n")
  (let ((test-segment
         (make-segment (make-point 1.5 7.5)
                       (make-point -1.5 -8.5))))
    (print-point (midpoint-segment test-segment))))

(main)


