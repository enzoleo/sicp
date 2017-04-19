## The solution of exercise 2.6
## In case representing pairs as procedures wasn't mind-boggling enough,
## consider that, in a language that can manipulate procedures, we can get
## by without numbers (at least insofar as nonnegative integers are
## concerned) by implementing 0 and the operation of adding 1 as
##
##     (defun zero (f) (lambda (x) x))
##     (defun add-1 (n)
##       (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))
##
## This representation is known as Church numerals, after its inventor,
## Alonzo Church, the logician who invented the calculus.
##
## Define one and two directly (not in terms of zero and add-1). (Hint:
## Use substitution to evaluate `(add-1 zero)`). Give a direct definition
## of the addition procedure `+` (not in terms of repeated application of
## `add-1`).
##
## -------- (above from SICP)
##

## The new-defined `numbers` in Church numerals
def zero(f): return lambda x: x
def one(f): return lambda x: f(x)
def two(f): return lambda x: f(f(x))

def add_one(n):
    return lambda f: lambda x: f(n(f)(x))

def church_plus(m, n):
    return lambda f: lambda x: m(f)(n(f)(x))


