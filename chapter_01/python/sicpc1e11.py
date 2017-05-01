## The solution of exercise 1.11
## A function f is defined by the rule that f(n) = n if n < 3 and f(n) =
## f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3) if n >= 3. Write a procedure
## that computes f by means of a recursive process. Write a procedure that
## computes f by means of an iterative process
##

def rec_f(n):
    """Recursive process"""
    if n <= 3:
        return n
    else:
        return (rec_f(n - 1) + \
                rec_f(n - 2) * 2 + \
                rec_f(n - 3) * 3)

def itr_f(n):
    """Iterative process"""
    if n <= 3:
        return n
    a, b, c = 1, 2, 3
    while n > 3:
        a, b, c, n = b, c, a * 3 + b * 2 + c, n - 1
    return c

# The iterative process and the recursive process return the same result
# in case the input parameter is integer. In other word, they return
# different results when the input parameter @n is not an integer.


