## The solution of exercise 1.21
## Use the `smallest-divisor` procedure to find the smallest divisor of
## each of the following numbers: 199, 1999, 19999
##

def smallest_divisor(n):
    """Find the smallest divisor of the given number n.
    Only positive integer input is allowed.
    
    For example, smallest_divisor(77) = 7.
    This function is used to check whether a number is a prime."""
    if not (isinstance(n, int) and n > 0):
        raise TypeError("only positive integer has smallest divisor")
    if n == 1:
        return 1
    a = 2
    while n % a:
        a += 1
        if a * a >= n:
            return n
    return a

#
#   >>> smallest_divisor(199)
#   199
#   >>> smallest_divisor(1999)
#   1999
#   >>> smallest_divisor(19999)
#   7
#


