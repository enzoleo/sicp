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
import math
import numbers
import operator

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

# Define a rational class.
# Notice that all rational numbers can be written as fraction form, so we
# can construct a rational number with its numerator and denominator.
class Rational:
    """This class implements rational numbers.

    In the two-argument form of the constructor, the fraction will do own
    deduction by itself, which means for example, Rational(10, -15) will 
    produce a rational number equivalent to -2/3. Both arguments muse be
    Rational. The numerator defaults to 0 and the denominator defaults to
    1 so that Rational() produces zero.
    """

    # We set the slots == ('__numerator', '__denominator') so only
    # attributes named numerator and denominator are allowed to be added
    # to this class. Actually the __numerator and __denominator is not
    # allowed to be accessed directly outside the class.
    __slots__ = ('__numerator', '__denominator')
    
    def __init__(self, numer = 0, denom = 1):
        """Initialize a rational number with numerator and denominator.
        The default rational number is zero (0/1).
        """
        if denom == 0:
            raise ZeroDivisionError("integer division or modulo by zero")
        self.__numerator   = numer
        self.__denominator = denom
        self.simplify()

    def simplify(self):
        """Simplify the rational to a proper fraction."""
        # Compute the greatest common divisor of numerator and denominator.
        # If one of the attributes numerator and denominator does not
        # exist, return.
        try:
            factor = gcd(self.__numerator, self.__denominator)
        except AttributeError:
            return
        
        if sys.version_info[0] < 3:
            self.__numerator /= factor
            self.__denominator /= factor
        else:
            self.__numerator = int(self.__numerator / factor)
            self.__denominator = int(self.__denominator / factor)

    @property
    def numerator(rat):
        return rat.__numerator

    @numerator.setter
    def numerator(rat, numer):
        rat.__numerator = numer
        rat.simplify()

    @property
    def denominator(rat):
        return rat.__denominator

    @denominator.setter
    def denominator(rat, denom):
        if denom == 0:
            raise ZeroDivisionError("integer division or modulo by zero")
        rat.__denominator = denom
        rat.simplify()

    def __repr__(self):
        """Print method"""
        # Print all attributes of this interval
        _repr_str = '%s/%s' % \
                    (self.__numerator, self.__denominator)
        return _repr_str

    def _operator_fallbacks(monomorphic_operator, fallback_operator):

        def forward(a, b):
            if isinstance(b, (int, Rational)):
                return monomorphic_operator(a, b)
            elif isinstance(b, float):
                return fallback_operator(float(a), b)
            elif isinstance(b, complex):
                return fallback_operator(complex(a), b)
            else:
                return NotImplemented
        forward.__name__ = '__' + fallback_operator.__name__ + '__'
        forward.__doc__ = monomorphic_operator.__doc__

        def reverse(b, a):
            if isinstance(a, Rational):
                # Includes ints.
                return monomorphic_operator(a, b)
            elif isinstance(a, numbers.Real):
                return fallback_operator(float(a), float(b))
            elif isinstance(a, numbers.Complex):
                return fallback_operator(complex(a), complex(b))
            else:
                return NotImplemented
        reverse.__name__ = '__r' + fallback_operator.__name__ + '__'
        reverse.__doc__ = monomorphic_operator.__doc__

        return forward, reverse
    
    def __eq__(self, rat):
        """Equality determination"""
        return self.numerator == rat.numerator and \
            self.denominator == rat.denominator

    def _add(self, rat):
        """Addition of rational numbers"""
        return Rational(self.denominator * rat.numerator + \
                        self.numerator * rat.denominator,
                        self.denominator * rat.denominator)

    __add__, __radd__ = _operator_fallbacks(_add, operator.add)

    def _sub(self, rat):
        """Subtraction of rational numbers"""
        return Rational(self.numerator * rat.denominator - \
                        self.denominator * rat.numerator,
                        self.denominator * rat.denominator)

    __sub__, __rsub__ = _operator_fallbacks(_sub, operator.sub)

    def _mul(self, rat):
        """Multiplication of rational numbers"""
        return Rational(self.numerator * rat.numerator,
                        self.denominator * rat.denominator)

    __mul__, __rmul__ = _operator_fallbacks(_mul, operator.mul)


    def div_interval(self, rat):
        """A rational divides a rational"""
        return Rational(self.numerator * rat.denominator, \
                        self.denominator * rat.numerator)

    __truediv__, __rtruediv__ = _operator_fallbacks(_div, \
                                                    operator.truediv)
    __div__, __rdiv__ = _operator_fallbacks(_div, operator.div)
    
    

