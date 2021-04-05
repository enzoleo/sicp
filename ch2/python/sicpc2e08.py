## The solution of exercise 2.8
## Using reasoning analogous to Alyssa's, describe how the difference of
## two intervals may be computed. Define a corresponding subtraction
## procedure, called `sub-interval`.
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

## Define the interval class.
class Interval:
    """Here we define a interval class with 2 attributes: lower-bound and
    upper-bound. To construct the instance of this class, the 2 arguments
    (@lower_bound and @upper_bound) are needed.

    Construct the interval with lower bound and upper bound directly. But
    the constructor will raise an error if your lower bound is larger
    than upper bound.    
    """

    __slots__ = ('__lb', '__ub')
    
    # The initialization with two parameters: lower and upper bounds
    def __init__(self, lb, ub):
        """Initialize the interval class.
        You must supply the lower and upper bounds of this interval.
        Add your lower_bound is not allowed to be larger than upper_bound.
        """
        # Notice that you can also import math module and use function
        # math.isnan() to check whether the lower bound or upper bound is
        # NaN. But here we do not use this method.
        if isnan(lb) or isnan(ub):
            raise ValueError("nan value of bounds occurs: %r, %r" % \
                             (lb, ub))
        if not (isinstance(lb, (int, float)) and \
                isinstance(ub, (int, float))):
            raise TypeError("cannot initialize interval with bounds "
                            "in wrong type: %s, %s" % \
                            (type(lb).__name__, type(ub).__name__))
        if lb > ub:
            raise ValueError("lower bound is larger than upper bound: "
                             "%r > %r" % (lb, ub))
        
        self.__lb, self.__ub = lb, ub

    @classmethod
    def from_number(self, number):
        """Initialize the interval class from a number.
        Notice that a number can be seen as an interval whose lower bound
        equals to upper bound.
        """
        if not isinstance(number, (int, float)):
            raise TypeError("cannot initialize interval with bounds "
                            "in wrong type: %s" % type(number).__name__)
        return Interval(number, number)

    @property
    def lb(itv):
        return itv.__lb

    @lb.setter
    def lb(itv, new_lb):
        if new_lb > itv.__ub:
            raise ValueError("lower bound is larger than upper bound: "
                             "%r > %r" % (new_lb, itv.__ub))
        itv.__lb = new_lb
    
    @property
    def ub(itv):
        return itv.__ub

    @ub.setter
    def ub(itv, new_ub):
        if new_ub < itv.__lb:
            raise ValueError("lower bound is larger than upper bound: "
                             "%r > %r" % (itv.__lb, new_ub))
        itv.__ub = new_ub

    @property
    def bounds(itv):
        return itv.__lb, itv.__ub

    @bounds.setter
    def bounds(itv, new_bounds):
        if new_bounds[0] < new_bounds[1]:
            raise ValueError("lower bound is larger than upper bound: "
                             "%r > %r" % (new_bounds[0], new_bounds[1]))
        itv.__lb, itv.__ub = new_bounds

    def __repr__(self):
        """Print method"""
        # Print all attributes of this interval
        return 'Interval \t[%s, %s]' % \
               (self.__lb, self.__ub)
            
    def __eq__(self, itv):
        """Equality determination"""
        # Only two intervals with completely the same attributes are
        # equal. The attributes of an interval are determined by the two
        # independent attributes named @lower_bound and @upper_bound.
        if isinstance(itv, Interval):
            return (self.lb == itv.lb and self.ub == itv.ub)
        else:
            return False
        
    def __add__(self, addend):
        """Interval Addition"""
        if isinstance(addend, Interval):
            return Interval(self.lb + addend.lb,
                            self.ub + addend.ub)
        elif isinstance(addend, (int, float)):
            return Interval(self.lb + addend,
                            self.ub + addend)
        return NotImplemented

    def __sub__(self, minuend):
        """Interval Subtraction"""
        if isinstance(minuend, Interval):
            return Interval(self.lb - minuend.ub,
                            self.ub - minuend.lb)
        elif isinstance(minuend, (int, float)):
            return Interval(self.lb - minuend,
                            self.ub - minuend)
        return NotImplemented
    
    def __mul__(self, multiplier):
        """Interval multiplication"""
        if isinstance(multiplier, Interval):
            pll = self.lb * multiplier.lb
            pul = self.ub * multiplier.lb
            plu = self.lb * multiplier.ub 
            puu = self.ub * multiplier.ub
            lb = min(pll, pul, plu, puu)
            ub = max(pll, pul, plu, puu)
            return Interval(lb, ub)
        elif isinstance(multiplier, (int, float)):
            if multiplier >= 0:
                return Interval(self.lb * multiplier,
                                self.ub * multiplier)
            else:
                return Interval(self.ub * multiplier,
                                self.lb * multiplier)
        return NotImplemented

    def _rdiv(self, other):
        """An interval divides a real number"""
        if isinstance(other, (int, float)):
            if isinf(other):
                return Interval(other, other)

            itv = Interval(1.0 / self.ub, 1.0 / self.lb)
            return itv.__mul__(other)
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
    


