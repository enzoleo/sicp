## The solution of exercise 1.8
## Newton's method for cube roots is based on the fact that if y is an
## approximation to the cube root of x, then a better approximation is
## given by the value (x / y ^ 2 + 2 * y) / 3.
##
## You can prove that give an initial value 1.0, then the formula above
## defines a sequence of approximate cube roots of x and it converges to
## the cube roots of x. So we use this formula to implement a cube-root
## procedure analogous to the square-root procedure.
## 

def improve(guess, x):
    """Improve guess.
    A guess is improved by averaging it with the quotient of the radicand
    and the old guess.
    """
    return (2 * guess + x * 1.0 / (guess * guess)) / 3

def good_enough(new_guess, old_guess):
    return abs((new_guess - old_guess * 1.0) / old_guess) < 0.0001

# The basic strategy as a procedure
# The thought is really simple, as we use recursive in definition
def curt_iter(init, x):
    """Compute the square root of x with initial value.
    Newton method is used here.
    """
    if x == 0:
        return 0
    guess, new_guess = init, improve(init, x)
    while not good_enough(new_guess, guess):
        new_guess, guess = improve(new_guess, x), new_guess
    return new_guess



