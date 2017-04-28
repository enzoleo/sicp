## The solution of exercise 1.7
## Square roots by Newton's method
## The `good-enough?` test in exercise 1.6 will not be very effective for
## finding the square roots of very small numbers (because the number used
## for comparison is always a positive number which means there always
## exists smaller numbers). Also, in real computers, arithmetic operations
## are almost always performed with limited precision. This makes our test
## inadequate for very large numbers.
##
## An alternative strategy for implementing `good-enough?` is to watch how
## `guess` changes from one iteration to the next and to stop when the
## change is a very small fraction of the guess. here we simply design a
## square-root procedure that uses this kind of end test.
## 

def average(x, y):
    return (x + y) / 2.0

def improve(guess, x):
    """Improve guess.
    A guess is improved by averaging it with the quotient of the radicand
    and the old guess.
    """
    return average(guess, x * 1.0 / guess)

def good_enough(new_guess, old_guess):
    return abs(new_guess - old_guess) * 1.0 / old_guess < 0.0001

# The basic strategy as a procedure
# The thought is really simple, as we use recursive in definition
def sqrt_iter(init, x):
    """Compute the square root of x with initial value.
    Newton method is used here.
    """
     if x < 0:
        raise ValueError("negative number %s has no real square root." % x)
    if x == 0:
        return 0
    guess, new_guess = init, improve(init, x)
    while not good_enough(new_guess, guess):
        new_guess, guess = improve(new_guess, x), new_guess
    return new_guess



