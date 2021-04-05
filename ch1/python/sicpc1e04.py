## The solution of exercise 1.4
## Observe that our model of evaluation allows for combinations whose
## operators are compound expressions. Use this observation to describe
## the behavior of the following procedure.

import operator

def plus_abs(a, b):
    """This function implements math function a + abs(b).
    But we do not use abs() function in python builtin module.
    We use add and sub operator in python operator module instead.
    """
    def op(x):
        """Return addition operator if the argument is positive and return
        subtraction operator if the argument is negative.
        """
        if x > 0:
            return operator.add
        else:
            return operator.sub
    return op(b)(a, b)

## Note how the function works.
## The expression above is equivalent to the math equation: a + abs(b)
## op(x) returns operator.add if b > 0 is true.


