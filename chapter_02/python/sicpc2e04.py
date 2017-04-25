## The solution of exercise 2.4
## Here is an alternative procedural representation of pairs. For this
## representation, verify that (new-car (new-cons x y)) yields x for any
## objects x and y.
##
##     (defun new-cons (x y)
##       (lambda (m) (funcall m x y)))
##     (defun new-car (z)
##       (funcall z (lambda (p q) p)))
##
## What is the corresponding definition of cdr?
##
## -------- (above from SICP)
##

def cons(x, y):
    return lambda m: m(x, y)

def car(z):
    return z(lambda p, q: p)

def cdr(z):
    return z(lambda p, q: q)


