## The solution of exercise 1.3
## Define a procedure that takes three numbers as arguments and returns
## the sum of the squares of the two larger numbers
##
## Input three numbers from keyboard
## Return the sum of the squares of the two larger numbers
##

def glss(a, b, c):
    """Find two largest numbers of the three parameters.
    And return the sum of square of them.
    """
    if a > b:
        if b > c:
            return (a ** 2 + b ** 2)
        else:
            return (a ** 2 + c ** 2)
    else:
        if a > c:
            return (a ** 2 + b ** 2)
        else:
            return (c ** 2 + b ** 2)


