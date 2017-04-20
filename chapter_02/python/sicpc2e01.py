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
        self.numer = numer
        self.denom = denom
        self.simplify()

    def simplify(self):
        """Simplify the rational to a proper fraction.
        """
        # Compute the greatest common divisor of numerator and denominator
        factor = gcd(self.numer, self.denom)
        self.numer /= factor
        self.denom /= factor

    def __repr__(self):
        """Print method
        """
        # Print all attributes of this interval
        ostr = "%d/%d" % \
               (self.numer, self.denom)
        return ostr
        

