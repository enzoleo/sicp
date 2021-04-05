## The solution of exercise 2.5
## Show that we can represent pairs of nonnegative integers using only
## numbers and arithmetic operations if we represent the pair a and b as
## the integer that is the product 2 ^ a * 3 ^ b . Give the corresponding
## definitions of the procedures cons, car, and cdr.
##
## -------- (above from SICP)
##

def cons(a, b):
    return ((2 ** a) * (3 ** b))

def mod_factor(n, factor):
    """This function computes the maximum numer m which satisfy:
    factor ** m | n. For example, in case where n == 486 and factor == 3,
    mod_factor(486, 3) returns 5 because 486 = 2 * 3 ** 5.
    """
    result = 0
    while n % factor == 0:
        n, result = n // factor, result + 1
    return result

def car(z):
    return mod_factor(z, 2)

def cdr(z):
    return mod_factor(z, 3)


