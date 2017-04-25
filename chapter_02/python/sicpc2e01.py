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
import operator

## Check the python version
## Write different code for different python version
if sys.version_info[0] < 3:
    # Use new type class
    __metaclass__ = type

    # Define functions to detect nan and infty value
    def isnan(a):
        """Check whether a is nan."""
        return a != a

    def isinf(a):
        """Check whether a is infty."""
        infty = float('inf')
        return (a == infty or a == -infty)

else:
    isnan = math.isnan
    isinf = math.isinf

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

    @classmethod
    def from_float(self, f):
        """Converts a finite float number to a rational number and return
        a Rational instance.

        Here we use as_interger_ratio() method in float module to convert
        a float number, thus the result is possible to be inaccurate. For
        example, you cannot get a rational number 7/10 from a float 0.7
        using Rational.from_float(0.7) but Rational(7, 10) is able to
        do such a thing.
        """
        if isinstance(f, int):
            # The case where f is an interger.
            return Rational(f, 1)
        elif not isinstance(f, float):
            # The case where f is not a float number
            raise TypeError("Rational.from_float() cannot take %r as an "
                            "argument because of wrong type: %s" % \
                            (f, type(f).__name__))
        if isnan(f) or isinf(f):
            raise TypeError("Cannot convert %r to Rational type." % f)
        return Rational(*f.as_integer_ratio())

    def to_float(self):
        """Converts a rational number to a float number."""
        return 1.0 * self.numerator / self.denominator

    def to_complex(self):
        """Converts a rational number to a complex number."""
        return complex(1.0 * self.numerator / self.denominator, 0)

    def simplify(self):
        """Simplify the rational to a proper fraction."""
        # Compute the greatest common divisor of numerator and
        # denominator. If one of the attributes numerator and denominator
        # does not exist, return.
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

    @property
    def fraction(rat):
        return rat.__numerator, rat.__denominator

    @fraction.setter
    def fraction(rat, frac):
        if frac[1] == 0:
            raise ZeroDivisionError("integer division or modulo by zero")
        rat.__numerator, rat.__denominator = frac
        rat.simplify()

    def __repr__(self):
        """Print method"""
        # Print all attributes of this interval.
        # For example, print rational number Rational(5,10), you get 1/2.
        _repr_str = '%s/%s' % \
                    (self.__numerator, self.__denominator)
        return _repr_str

    def _operator_fallbacks(monomorphic_operator, fallback_operator):
        """Generates forward and reverse operators given a purely-rational
        operator and a function from the operator module.

        This function is inspired by the builtin module fractions.py.
        To locate this file fractions.py, you can enter python env by
        typing `python -v`, and then `from fractions import Fraction`.
        The interpreter will tell you the path of this fractions module.

        We want our __add__, __sub__, __mul__ and __div__ are all able to
        do arithmetic operations with different types of arguments. For
        example, given r = Rational(5, 10) (which means r == 1/2), we want
        to implement the following additions:
        
            r + r = 1/1
            r + 1 = 3/2
            r + 1.5 = 2.0
            r + (1.2 + 5j) = 1.7 + 5j
            1 + r = 3/2
            (1.2 + 5j) + r = 1.7 + 5j

        Of course the following methods are available but we still want
        much more simple definitions for subtraction, multiplication and
        division.
        
            def __add__(self, rat):
                if isinstance(rat, (int, Rational)):
                    return Rational(self.numerator * rat.denominator +
                                    rat.numerator * self.denominator,
                                    self.denominator * rat.denominator)
                elif isinstance(other, float):
                    return self.to_float() + other
                elif isinstance(other, complex):
                    return self.to_complex() + other
                return NotImplemented

            def __radd__(self, other):
                if isinstance(other, int):
                    return Rational(self.numerator * other.denominator +
                                    other.numerator * self.denominator,
                                    self.denominator * other.denominator)
                elif isinstance(other, float):
                    return other + self.to_float()
                elif isinstance(other, complex):
                    return other + self.to_complex()
                return NotImplemented

        Thus we generate forward and reverse operators for __add__ and
        __radd__ (or subtraction or other). Here `operator` module is
        needed.
        """
        def forward(a, b):
            if isinstance(b, (int, Rational)):
                return monomorphic_operator(a, b)
            elif isinstance(b, float):
                return fallback_operator(a.to_float(), b)
            elif isinstance(b, complex):
                return fallback_operator(a.to_complex(), b)
            else:
                return NotImplemented
        forward.__name__ = '__' + fallback_operator.__name__ + '__'
        forward.__doc__ = monomorphic_operator.__doc__

        def reverse(b, a):
            if isinstance(a, int):
                return monomorphic_operator(a, b)
            elif isinstance(a, float):
                return fallback_operator(a, b.to_float())
            elif isinstance(a, complex):
                return fallback_operator(a, b.to_complex())
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


    def _div(self, rat):
        """A rational divides a rational"""
        return Rational(self.numerator * rat.denominator, \
                        self.denominator * rat.numerator)

    if sys.version_info[0] < 3:
        __div__, __rdiv__ = _operator_fallbacks(_div, operator.div)
    else:
        __truediv__, __rtruediv__ = _operator_fallbacks(_div, \
                                                        operator.truediv)
    
    

