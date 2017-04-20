## The solution of exercise 2.8
## Using reasoning analogous to Alyssa's, describe how the difference of
## two intervals may be computed. Define a corresponding subtraction
## procedure, called `sub-interval`.
##
## -------- (above from SICP)
##

import sys

## Check the python version
## Write different code for different python version
if sys.version_info[0] < 3:
    # Use new type class
    __metaclass__ = type

##
## Define the interval class.
## Here we define a interval class with 2 attributes: lower-bound and
## upper-bound. To construct the instance of this class, the 2 arguments
## (@lower_bound and @upper_bound) are needed.
##
class Interval:

    # Infinity number only in class Interval
    infty = float('inf')

    # The initialization with two parameters: lower and upper bounds
    def __init__(self, lb, ub):
        """Initialize the interval class.
        You must supply the lower and upper bounds of this interval.
        """
        # Notice that you can also import math module and use function
        # math.isnan() to check whether the lower bound or upper bound is
        # NaN. But here we do not use this method.
        assert lb >= -self.infty and ub <= self.infty, \
            "NaN value of bounds occurs!"
        assert lb <= ub, "Lower bound is larger than upper bound!"
        
        self.lower_bound = lb
        self.upper_bound = ub

    def get_bounds(self):
        """Return the lower and upper bounds"""
        return self.lower_bound, self.upper_bound

    def set_bounds(self, bounds):
        """Reset the center and percent"""
        assert bounds[0] <= bounds[1], \
            "Lower bound is larger than upper bound!"
        self.lower_bound, self.upper_bound = bounds

    # Property function
    bounds = property(get_bounds, set_bounds)

    def __repr__(self):
        """Print method"""
        # Print all attributes of this interval
        ostr = "Interval \t[%s, %s]" % \
               (self.lower_bound, self.upper_bound)
        return ostr
            
    def __eq__(self, itv):
        """Equality determination"""
        # Only two intervals with completely the same attributes are
        # equal. The attributes of an interval are determined by the two
        # independent attributes named @lower_bound and @upper_bound.
        return (self.lower_bound == itv.lower_bound and \
                self.upper_bound == itv.upper_bound)
        
    def __add__(self, itv):
        """Interval Addition"""
        infty = self.infty

        new_lb = self.lower_bound + itv.lower_bound
        new_ub = self.upper_bound + itv.upper_bound

        # The value of lower_bound and upper_bound could not be nan,
        # which means they must satisfy the following inequalities.
        assert -infty <= new_lb and new_ub <= infty, \
            "NaN value of bounds occurs!"
        
        return Interval(new_lb, new_ub)

    def __sub__(self, itv):
        """Interval subtraction"""
        infty = self.infty
        
        new_lb = self.lower_bound - itv.upper_bound
        new_ub = self.upper_bound - itv.lower_bound

        # The value of lower_bound and upper_bound could not be nan,
        # which means they must satisfy the following inequalities.
        assert -infty <= new_lb and new_ub <= infty, \
            "NaN value of bounds occurs!"
        
        return Interval(new_lb, new_ub)
    
    def __mul__(self, itv):
        """Interval multiplication"""
        pll = self.lower_bound * itv.lower_bound
        pul = self.upper_bound * itv.lower_bound
        plu = self.lower_bound * itv.upper_bound 
        puu = self.upper_bound * itv.upper_bound

        lb = min(pll, pul, plu, puu)
        ub = max(pll, pul, plu, puu)
        return Interval(lb, ub)

    def rdiv_interval(self, num):
        """An interval divides a real number"""
        if num == self.infty or num == -self.infty:
            return Interval(num, num)
        
        if num > 0:
            return Interval(num * 1.0 / self.upper_bound, \
                            num * 1.0 / self.lower_bound)
        else:
            return Interval(num * 1.0 / self.lower_bound, \
                            num * 1.0 / self.upper_bound)
        return Interval(0, 0)

    def div_interval(self, itv):
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
        __rtruediv__ = rdiv_interval
        __truediv__  =  div_interval
    else:
        __rdiv__ = rdiv_interval
        __div__  =  div_interval
    


