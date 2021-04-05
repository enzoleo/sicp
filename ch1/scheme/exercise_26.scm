;; The solution of exercise 1.26
;; Louis Reasoner is having great difficulty doing exercise 1.24. His
;; `fast-prime?` test seems to run more slowly than his `prime?` test.
;; Louis calls his friend over to help. When they examine Louis's code,
;; they find that he has rewritten the expmod procedure to use an explicit
;; multiplication, rather than calling `square`.
;;
;; His friend found that by writing the procedure like that, he transformed
;; the O(log n) process into a O(n) process. Explain.
;;
;; -------- (above from SICP)
;;

;; The modified `expmod` procedure
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((= 0 (modulo exp 2))
         (modulo (* (expmod base (/ exp 2) m)
                    (expmod base (/ exp 2) m))
                 m))
        (else
         (modulo (* base (expmod base (- exp 1) m))
                 m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 20))

(define (next-prime n)
  (define (test-prime counter)
    (if (prime? counter)
        counter
        (test-prime (+ counter 1))))
  (test-prime (+ n 1)))

(define (search-for-next-primes n m)
  (define (search-count init-num counter)
    (if (< counter m)
        (begin
          (let ((new-prime (next-prime init-num)))
          (display "prime[") (display counter) (display "] ")
          (display new-prime)
          (newline)
          (search-count new-prime (+ counter 1))))))
  (search-count n 0))

;;
;; Actually not every implementation has the procedure `runtime`. You can
;; use alternatives for different interpreter:
;;
;; [1] guile:
;;     (define (runtime) (tms:clock (times)))
;;
;; [2] racket
;;     (define (runtime) (current-milliseconds))
;;
;; In mit-scheme, use (runtime) directly.
;;
(define (runtime) (tms:clock (times)))

;; Compute the runtime of @search-for-next-primes
(define (compute-runtime n m)
  (define (clause-runtime start-time)
    (search-for-next-primes n m)
    (display "runtime: ")
    (display (- (runtime) start-time)))
  (clause-runtime (runtime))
  (newline))

;;
;; The most important point is the `expmod` procedure.
;; The original `expmod` reduces the computation time when @exp is even.
;; But the `expmod` by Louis computes (expmod base (/ exp 2) m) twice for
;; every number no matter it is odd or even.
;;
(define (main)
  (display "Find the three smallest primes larger than n.\n")
  (display "Try some large numbers and compare the result.\n")
  (display "Here we set n = 10 ^ 4, 10 ^ 5, 10 ^ 6.\n")
  (compute-runtime 1000 3) (newline)
  (compute-runtime 10000 3) (newline)
  (compute-runtime 100000 3) (newline))

(main)


