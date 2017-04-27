## The solution of exercise 1.6
## Square roots by Newton's method
##
## As a case in point, consider the problem of computing square roots.
## We can define the square-root function as:
##     square(x) = the y, such that y >= 0 and y^2 = x
## This describes a perfectly legitimate mathematical function. We could
## use it to recognize whether one number is the square root of another,
## or to describe a procedure. Indeed, it tells us almost nothing about
## how to actually find the square root of a given number. It will not
## help matters to rephrase this definition in pseudo-Lisp:
## 
##     (define (sqrt x)
##       (the y (and (>= y 0)
##                   (= (square y) x))))
##
## This only begs the question.
## -------- (above from SICP)
##
## How does one compute square roots? The most common way is to use
## Newton's method of successive approximations.
##

def average(x, y):
    return (x + y) / 2.0

def improve(guess, x):
    """Improve guess.
    A guess is improved by averaging it with the quotient of the radicand
    and the old guess.
    """
    return average(guess, x * 1.0 / guess)

## It is important to point out what we mean by `good enough`. Here we
## give a simple illustration (but it is not really a very good test).
## The idea is to improve the answer until it is close enough so that its
## square differs from the radicand by less than a predetermined tolerance
## (here 0.0001 is set)
## See exercise_07.scm to get a better good-enough? test.
def good_enough(guess, x):
    return abs(guess * guess - x) < 0.0001

def sqrt_iter(init, x):
    """Compute the square root of x with initial value.
    Newton method is used here.
    """
    guess = init
    while not good_enough(guess, x):
        guess = improve(guess, x)
    return guess




