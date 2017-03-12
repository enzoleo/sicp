;; The solution of exercise 2.1
;; Data-abstraction barriers in the rational-number package.
;; This simple idea has many advantages. One advantage is that it makes
;; programs much easier to maintain and to modify. Any complex data
;; structure can be represented in a variety of ways with the primitive
;; data structures provided by a programming language. Of course, the
;; choice of representation influences the programs that operate on it;
;; thus, if the representation were to be changed at some later time, all
;; such programs might have to be modified accordingly. This task could be
;; time-consuming and expensive in the case of large programs unless the
;; dependence on the representation were to be confined by design to a
;; very few program modules. For example, an alternate way to address the
;; problem of reducing rational numbers to lowest terms is to perform the
;; reduction whenever we access the parts of a rational number, rather
;; than when we construct it. This leads to different constructor and
;; selector procedures:
;;
;;     (defun make-rat (n d)
;;       (cons n d))
;;     (defun numer (x)
;;       (let ((g (gcd (car x) (cdr x))))
;;         (/ (car x) g)))
;;     (defun denom (x)
;;       (let ((g (gcd (car x) (cdr x))))
;;         (/ (cdr x) g)))
;;
;; The difference between this implementation and the previous one lies in
;; when we compute the `gcd`. If in our typical use of rational numbers we
;; access the numerators and denominators of the same rational numbers
;; many times, it would be preferable to compute the gcd when the rational
;; numbers are constructed. If not, we may be better off waiting until
;; access time to compute the gcd. In any case, when we change from one
;; representation to the other, the procedures add-rat, sub-rat, and so on
;; do not have to be modified at all. Constraining the dependence on the
;; representation to a few interface procedures helps us design programs
;; as well as modify them, because it allows us to maintain the flexibility
;; to consider alternate implementations. To continue with our simple
;; example, suppose we are designing a rational-number package and we can't
;; decide initially whether to perform the gcd at construction time or at
;; selection time. The data-abstraction methodology gives us a way to defer
;; that decision without losing the ability to make progress on the rest of
;; the system.
;;
;; -------- (above from SICP)
;;


