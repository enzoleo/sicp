## After considerable work, Alyssa P. Hacker delivers her finished system.
## Several years later, after she has forgotten all about it, she gets a
## frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has
## noticed that the formula for parallel resistors can be written in two
## algebraically equivalent ways:
##
##      R_1 * R_2                         1
##     -----------      and      -------------------
##      R_1 + R_2                 1 / R_1 + 1 / R_2
##
## He has written the following two programs, each of which computes the
## parallel-resistors formula differently:
##
##     (define (par1 r1 r2)
##         (div-interval (mul-interval r1 r2)
##                       (add-interval r1 r2)))
##     (define (par2 r1 r2)
##         (let ((one (make-interval 1 1)))
##           (div-interval one
##                         (add-interval (div-interval one r1)
##                                       (div-interval one r2)))))
##
## Lem complains that Alyssa's program gives different answers for the two
## ways of computing. This is a serious complaint.
##
## The solution of exercise 2.14
## Demonstrate that Lem is right. Investigate the behavior of the system
## on a variety of arithmetic expressions. Make some intervals A and B,
## and use them in computing the expressions A / A and A / B. You will get
## the most insight by using intervals whose width is a small percentage of
## the center value. Examine the results of the computation in center-
## percent form (see exercise 2.12).
##
## -------- (above from SICP)
##

import sys
import math

## Check the python version
## Write different code for different python version
if sys.version_info[0] < 3:
    # Use new type class
    __metaclass__ = type

    infty = float('inf')
    # Define functions to detect nan and infty value
    def isnan(a):
        """Check whether a is nan."""
        return a != a

    def isinf(a):
        """Check whether a is infty."""
        return (a == infty or a == -infty)

else:
    infty = math.inf
    isnan = math.isnan
    isinf = math.isinf
    
## Define the interval class.
class Interval:
    """Here we define a interval class with 2 attributes: center and
    percent, which is a little different from that in exercise 2.11.

    To construct the instance of this class, only 2 arguments (@center and
    @percent) are needed, and the contructor will automatically compute
    the other attributes. But you can also construct the instance from a
    number (an interval with the same bounds) or from a pair of its lower
    and upper bounds.

      - From center and percent:
        Interval(5, 20) produces interval [4, 6]

      - From bounds:
        Interval.from_bounds(-4, 7) produces interval [-4, 7]

      - From number:
        Interval.from_number(100) produces interval [100, 100]
        Interval.from_number(math.inf) produces interval [inf, inf]
    """

    __slots__ = ('__center', '__percent')
    
    # The initialization with two parameters: center and percent.
    # Still allowed initialization with bounds or a number.
    def __init__(self, center, percent):
        """Initialize the interval class.
        You must supply the center and the percent of this interval.
        The percent parameter must be positive.
        Only int and float type is allowed here.
        """
        if not (isinstance(center, (int, float)) and \
                isinstance(percent, (int, float))):
            raise TypeError("cannot initialize interval with parameters "
                            "in wrong type: %s, %s" % \
                            (type(center).__name__, \
                             type(percent).__name__))
        if isnan(center) or isnan(percent):
            raise ValueError("nan value of parameters occurs: %r, %r" % \
                             (center, percent))
        
        self.__center = center
        self.__percent = percent
        self.__update()

    @classmethod
    def from_bound(self, lb, ub):
        """Initialize the interval class.
        You must supply the lower and upper bounds of this interval.
        
        Your lower_bound is not allowed to be larger than upper_bound, and
        you cannot construct an interval whose center == 0, with positive
        width, such as [-5, 5], because there does not exist such an
        interval in (center, percent) form.

        You cannot construct an interval whose one bound is finite while
        the other is infinite because of the same reason.
        """
        if not (isinstance(lb, (int, float)) and \
                isinstance(ub, (int, float))):
            raise TypeError("cannot initialize interval with bounds "
                            "in wrong type: %s, %s" % \
                            (type(lb).__name__, type(ub).__name__))
        if isnan(lb) or isnan(ub):
            raise ValueError("nan value of bounds occurs: %r, %r" % \
                             (lb, ub))
        if lb > ub:
            raise ValueError("lower bound is larger than upper bound: "
                             "%r > %r" % (lb, ub))
        if lb + ub == 0 and lb != 0:
            raise ValueError("finite non-zero bounds: %r, %r "
                             "and zero center." % (lb, ub))
        if (isinf(lb) and not isinf(ub)) or \
           (isinf(ub) and not isinf(lb)):
            raise ValueError("one bound is finite while the other is "
                             "infinite: %r, %r" % (lb, ub))

        # In this case, the upper bound is +inf and the lower bound is
        # -inf, which means the interval is R := (-inf, +inf). Here we
        # simply set the center zero because operation `ub + lb`
        # returns nan.
        if ub == infty and lb == -infty:
            return Interval(0, infty)
        elif ub == lb == 0:
            return Interval(0, 0)
        
        center = (ub + lb) / 2.0
        percent = abs(50.0 * (ub - lb) / center)
        if isnan(percent):
            percent = 0
        return Interval(center, percent)

    @classmethod
    def from_number(self, number):
        """Initialize the interval class. You must supply the number.
        Here we create an interval instance whose lower bound equals
        its upper bound.
        """
        if not isinstance(number, (int, float)):
            raise TypeError("cannot initialize interval with bounds "
                            "in wrong type: %s" % type(number).__name__)
        return Interval(number, 0)

    @property
    def center(interval):
        return interval.__center

    @center.setter
    def center(interval, nc):
        if not isinstance(nc, (int, float)):
            raise TypeError("cannot initialize interval with parameters "
                            "in wrong type: %s" % type(nc).__name__)
        interval.__center = nc
        interval.__update()
    
    @property
    def percent(interval):
        return interval.__percent

    @percent.setter
    def percent(interval, np):
        if not isinstance(np, (int, float)):
            raise TypeError("cannot initialize interval with parameters "
                            "in wrong type: %s" % type(np).__name__)
        interval.__percent = np
        interval.__update()

    @property
    def width(interval):
        if isinf(interval.__center):
            return 0
        if isinf(interval.__percent):
            return infty
        return abs(interval.__center * interval.__percent) / 100.0

    @property
    def lb(interval):
        return interval.__center - interval.width

    @property
    def ub(interval):
        return interval.__center + interval.width

    def __update(self):
        """Update attributes in Interval class.
        Here all these attributes have float datatype
        """
        if self.__percent < 0:
            raise ValueError("parameter percent must be positive.")
        if self.__center == 0:
            self.__percent = 0
        elif isinf(self.__center):
            if isinf(self.__percent):
                raise ValueError("cannot construct interval with "
                                 "infinite center and infinite percent.")
            self.__percent = 0
        else:
            if isinf(self.__percent):
                self.__center = 0

    @property
    def bounds(interval):
        return interval.lb, interval.ub

    def __repr__(self):
        """Print method"""
        # Print all attributes of this interval
        ostr = "Interval \t[%s, %s]\n" % \
               (self.lower_bound, self.upper_bound) + \
               "center   \t%s\npercent  \t%s\nwidth    \t%s" % \
               (self.center, self.percent, self.width)
        return ostr

    def __repr__(self):
        """Print method"""
        # Print all attributes of this interval
        return 'Interval \t[%s, %s]\n' % \
               (self.lb, self.ub) + \
               "center   \t%s\npercent  \t%s\nwidth    \t%s" % \
               (self.center, self.percent, self.width)
            
    def __eq__(self, itv):
        """Equality determination"""
        # Only two intervals with completely the same attributes are
        # equal. The attributes of an interval are determined by the two
        # independent attributes named @center and @percent.
        if isinstance(itv, Interval):
            return (self.center  == itv.center and \
                    self.percent == itv.percent)
        else:
            return False

    def __add__(self, addend):
        """Interval Addition"""
        if isinstance(addend, Interval):
            nc = self.center + addend.center
            np = (self.percent * abs(self.center) \
                  + addend.percent * abs(addend.center)) \
                  / (1.0 * abs(self.center + addend.center))
            return Interval(nc, np)
        elif isinstance(addend, (int, float)):
            nc = self.center + addend
            np = abs(self.center / nc) * self.percent
            return Interval(nc, np)
        return NotImplemented

    def __radd__(self, other):
        """Right addition"""
        if isinstance(other, (int, float)):
            return self.__add__(other)
        return NotImplemented

    def __sub__(self, minuend):
        """Interval subtraction"""
        if isinstance(minuend, Interval):
            nc = self.center - minuend.center
            np = (self.percent * abs(self.center) \
                  + minuend.percent * abs(minuend.center)) \
                  / (1.0 * abs(self.center - minuend.center))
            return Interval(nc, np)
        elif isinstance(minuend, (int, float)):
            nc = self.center - minuend
            np = abs(self.center / nc) * self.percent
            return Interval(nc, np)
        return NotImplemented

    def __rsub__(self, other):
        """Right subtraction"""
        if isinstance(other, (int, float)):
            return self.__mul__(-1).__add__(other)
        return NotImplemented

    def __mul__(self, multiplier):
        """Interval multiplication"""
        if isinstance(multiplier, Interval):
            if multiplier.percent >= 100 and \
               multiplier.percent > self.percent:
                nc = (1 + self.percent / 100.0) * \
                     self.center * multiplier.center
                return Interval(nc, multiplier.percent)
            elif multiplier.percent < 100 and self.percent < 100:
                nc = (1 + self.percent * multiplier.percent / 10000.0) * \
                     self.center * multiplier.center
                np = (10000.0 * (self.percent + multiplier.percent)) / \
                     (10000.0 + (self.percent * multiplier.percent))
                return Interval(nc, np)
            else:
                nc = (1 + multiplier.percent / 100.0) * \
                     self.center * multiplier.center
                return Interval(nc, self.percent)
        elif isinstance(multiplier, (int, float)):
            return Interval(self.center * multiplier, \
                            self.percent)
        return NotImplemented

    def __rmul__(self, other):
        """Right subtraction"""
        if isinstance(other, (int, float)):
            return self.__mul__(other)
        return NotImplemented

    def _rdiv(self, other):
        """An interval divides a real number"""
        # Notice that zero cannot be denominator and one interval spans
        # zero if and only if its percent >= 100
        if isinstance(other, (int, float)):
            if self.percent >= 100:
                raise ZeroDivisionError("the dividend interval spans "
                                        "zero")
            
            nc = 10000.0 * other / \
                 ((10000.0 - self.percent ** 2) * self.center)
            return Interval(nc, self.percent)
        return NotImplemented

    def _div(self, itv):
        """An interval divides another interval"""
        return self.__mul__(1 / itv)
        
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
        __rtruediv__ = _rdiv
        __truediv__  =  _div
    else:
        __rdiv__ = _rdiv
        __div__  =  _div

def par1(r1, r2):
    """The formula to compute parallel resistance"""
    return (r1 * r2) / (r1 + r2)

def par2(r1, r2):
    """The formula to compute parallel resistance"""
    return (1 / ((1 / r1) + (1 / r2)))
    


