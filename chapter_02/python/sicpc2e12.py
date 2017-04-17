## After debugging her program, Alyssa shows it to a potential user, who
## complains that her program solves the wrong problem. He wants a program
## that can deal with numbers represented as a center value and an
## additive tolerance; for example, he wants to work with intervals such
## as `3.5 +- 0.15` rather than [3.35, 3.65]. Alyssa returns to her desk
## and fixes this problem by supplying an alternate constructor and
## alternate selectors:
##
##     (define (make-center-width c w)
##       (make-interval (- c w) (+ c w)))
##     (define (center i)
##       (/ (+ (lower-bound i) (upper-bound i)) 2))
##     (define (width i)
##       (/ (- (upper-bound i) (lower-bound i)) 2))
##
## Unfortunately, most of Alyssa's users are engineers. Real engineering
## situations usually involve measurements with only a small uncertainty,
## measured as the ratio of the width of the interval to the midpoint of
## the interval. Engineers usually specify percentage tolerances on the
## parameters of devices, as in the resistor specifications given earlier.
##
## The solution of exercise 2.12
## Define a constructor make-center-percent that takes a center and a
## percentage tolerance and produces the desired interval. You must also
## define a selector percent that produces the percentage tolerance for a
## given interval. The center selector is the same as the one shown above.
##
## -------- (above from SICP)
##

import sys

## Check the python version
## Write different code for different python version
if sys.version_info[0] < 3:
    # Use new type class
    __metaclass__ = type

class Interval:

    # Infinity number only in class Interval
    infty = float('inf')

    # The initialization with two parameters: center and percent.
    # Still allowed initialization with bounds or a number.
    def __init__(self, center, percent):
        """
        Initialize the interval class.
        You must supply the center and the percent of this interval.
        The other attributes will be computed automatically.
        """
        self.center = center
        
        assert percent >= 0, "Negative percent is not allowed!"
        self.percent = percent
        self.update()

    @classmethod
    def from_bound(self, lb, ub):
        """
        Initialize the interval class.
        You must supply the lower bound and the upper bound.
        """
        infty = self.infty
        
        assert lb <= ub, "Lower bound is larger than upper bound!"
        assert not (lb + ub == 0 and lb != 0), \
            "Finite non-zero bounds and zero center!"
        assert not ((lb == -infty and -infty < ub < infty) \
                or  (ub ==  infty and -infty < lb < infty)), \
            "One bound is finite while the other is infinite!"

        if ub == infty and lb == -infty:
            # In this case, the upper bound is +inf and the lower bound is
            # -inf, which means the interval is R := (-inf, +inf). Here we
            # simply set the center zero because operation `ub + lb`
            # returns nan.
            center = 0
            percent = infty
            return Interval(center, percent)
        elif ub == lb == 0:
            return Interval(0, 0)
        else:
            center = (ub + lb) / 2

        if ub == lb == infty or ub == lb == -infty:
            # In this case, the two bounds are both inf, which means the
            # interval is sactually equivalent to the infinity. Here we
            # simply set the width zero because `ub - lb` returns nan.
            width = 0
        else:
            width = (ub - lb) / 2

        percent = abs(1.0 * width / center) * 100.0
        return Interval(center, percent)

    @classmethod
    def from_number(self, number):
        """
        Initialize the interval class. You must supply the number.
        Here we create an interval instance whose lower bound equals
        its upper bound.
        """
        center = number
        percent = 0
        return Interval(center, percent)

    def update(self):
        """
        Update attributes in Interval class.
        Here all these attributes have float datatype
        """
        infty = self.infty
        
        # The center and percent are not allowed to be infty
        # simultaneously.
        if abs(self.center) == infty:
            self.percent = 0

        # Updator will not work normally if the center is zero and the
        # percentage is infinity because the value of width is not
        # computable.
        if self.center == 0 and self.percent == infty:
            self.width = infty
            self.lower_bound = -infty
            self.upper_bound = infty
        else:
            self.width = abs(self.center) * self.percent / 100.0
            self.lower_bound = self.center - self.width
            self.upper_bound = self.center + self.width

    def __repr__(self):
        """
        Print method
        """
        # Print all attributes of this interval
        ostr = "Interval \t[%s, %s]\n" % \
               (self.lower_bound, self.upper_bound) + \
               "center   \t%s\npercent  \t%s\nwidth    \t%s" % \
               (self.center, self.percent, self.width)
        return ostr
            
    def __eq__(self, itv):
        """
        Equality determination
        """
        # Only two intervals with completely the same attributes are
        # equal. The attributes of an interval are determined by the two
        # independent attributes named center and percent.
        return (self.center  == itv.center and \
                self.percent == itv.percent)
        
    def __add__(self, itv):
        """
        Interval Addition
        """
        infty = self.infty
        
        # If one addend interval has infinite width, the width of the sum
        # interval is also infinite
        if self.width == infty or itv.width == infty:
            return Interval(-infty, infty)
            
        new_center = self.center + itv.center
        new_percent = (self.percent * abs(self.center) \
                       + itv.percent * abs(itv.center)) \
                       / (1.0 * abs(self.center + itv.center))
        return Interval(new_center, new_percent)

    def __sub__(self, itv):
        """
        Interval subtraction
        """
        infty = self.infty
        
        # If the subtractor interval or the minuend interval has infinite
        # width, the width of the difference interval is also infinite
        if self.width == infty or itv.width == infty:
            return Interval(-infty, infty)
        
        new_center = self.center - itv.center
        new_percent = (self.percent * abs(self.center) \
                       + itv.percent * abs(itv.center)) \
                       / (1.0 * abs(self.center - itv.center))
        return Interval(new_center, new_percent)

    def __mul__(self, itv):
        """
        Interval multiplication
        """
        infty = self.infty

        # If one of the factor interval has infinite width, the width of
        # the product interval is also infinite
        if self.width == infty or itv.width == infty:
            return Interval(-infty, infty)

        if itv.percent >= 100 and self.percent < itv.percent:
            new_center = (1 + self.percent / 100.0) * \
                         self.center * itv.center
            return Interval(new_center, itv.percent)
        elif itv.percent < 100 and self.percent < 100:
            new_center = (1 + self.percent * itv.percent / 10000.0) * \
                         self.center * itv.center
            new_percent = (10000.0 * (self.percent + itv.percent)) / \
                          (10000.0 + (self.percent * itv.percent))
            return Interval(new_center, new_percent)
        else:
            new_center = (1 + itv.percent / 100.0) * \
                         self.center * itv.center
            return Interval(new_center, self.percent)

    def rdiv_interval(self, num):
        """
        An interval divides a real number
        """
        # Notice that zero cannot be denominator and one interval spans
        # zero if and only if its percent >= 100
        assert self.percent < 100, \
            "The dividend interval spans zero!"

        if num == self.infty or num == -self.infty:
            return Interval(num, num)
        
        center  = self.center
        percent = self.percent
        
        new_center = 10000.0 * num / \
                     ((10000.0 - percent * percent) * center)
        return Interval(new_center, percent)

    def div_interval(self, itv):
        """
        An interval divides another interval
        """
        # Notice that zero cannot be denominator and one interval spans
        # zero if and only if its percent >= 100
        assert itv.percent < 100, \
            "The dividend interval spans zero!"
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


    


