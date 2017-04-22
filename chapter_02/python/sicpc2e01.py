## The solution of exercise 2.1
## Define a better version of make-rat that handles both positive and
## negative arguments. Make-rat should normalize the sign so that if the
## rational number is positive, both the numerator and denominator are
## positive, and if the rational number is negative, only the numerator is
## negative.
##
## -------- (above from SICP)
##

import sys

## Check the python version
## Write different code for different python version
if sys.version_info[0] < 3:
    # Use new type class
    __metaclass__ = type

# Calculate the greatest common divisor of two numbers.
def gcd(a, b):
    """Calculate the Greatest Common Divisor of a and b.
    The result will have the same sign as b unless b == 0.
    """
    while b:
        a, b = b, a % b
    return a

# Define a rational class
class Rational:
    
   def __init__(self, numer = 0, denom = 1):
        """Initialize a rational number with numerator and denominator.
        The default rational number is zero (0 / 1).
        """
        if denom == 0:
            raise ZeroDivisionError("integer division or modulo by zero")
        self.numer = numer
        self.denom = denom

    def simplify(self):
        """Simplify the rational to a proper fraction."""
        # Compute the greatest common divisor of numerator and denominator.
        # If one of the attributes numer and denom does not exist, return.
        try:
            factor = gcd(self.numer, self.denom)
        except AttributeError:
            return

        # Here we use __dict__ directly, because we cannot use assignment
        # statements inside method `simplify` (or it will throw recursion
        # error).
        if sys.version_info[0] < 3:
            self.__dict__['numer'] /= factor
            self.__dict__['denom'] /= factor
        else:
            self.__dict__['numer'] = int(self.numer / factor)
            self.__dict__['denom'] = int(self.denom / factor)

    def __repr__(self):
        """Print method"""
        # Print all attributes of this interval
        ostr = "%d/%d" % \
               (self.numer, self.denom)
        return ostr
    
    def __setattr__(self, name, value):
        """Overload attribute set method."""
        if name == 'denom' and value == 0:
            raise ZeroDivisionError("integer division or modulo by zero")
        self.__dict__[name] = value
        self.simplify()

    def __eq__(self, rat):
        """Equality determination"""
        return self.numer == rat.numer and self.denom == rat.denom

    def __add__(self, rat):
        """Addition of rational numbers"""
        return Rational(self.denom * rat.numer + self.numer * rat.denom,
                        self.denom * rat.denom)

    def __sub__(self, rat):
        """Subtraction of rational numbers"""
        return Rational(self.numer * rat.denom - self.denom * rat.numer,
                        self.denom * rat.denom)

    def __mul__(self, rat):
        """Multiplication of rational numbers"""
        return Rational(self.numer * rat.numer,
                        self.denom * rat.denom)

    def rdiv_interval(self, num):
        """A rational divides a rational"""
        return Rational(num * self.denom, self.numer)

    def div_interval(self, rat):
        """A rational divides a rational"""
        return Rational(self.numer * rat.denom, self.denom * rat.numer)

    # Here we check the current python version. If the current Python
    # environment is Python 3.x, we need to overload __rtruediv__ and
    # __truediv__ instead of __rdiv__ and __div__. However, in Python 2.x,
    # __rdiv__ and __div__ need to be overloaded to implement division
    # overloading.
    #
    # The old Python 2 `/` operator is replaced by the Python 3 behaviour
    # with the `from __future__` import, but here we do not import it.
    # In Python 3, all numeric division with operator `/` results in a
    # float result while it does not do that in Python 2.
    if sys.version_info[0] >= 3:
        __rtruediv__ = rdiv_interval
        __truediv__  =  div_interval
    else:
        __rdiv__ = rdiv_interval
        __div__  =  div_interval
    
    

