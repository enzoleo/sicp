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
if sys.version < '3':
    __metaclass__ = type

class Interval:

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
        assert lb <= ub, "Lower bound is larger than upper bound!"
        assert not (lb + ub == 0 and lb != 0), \
            "Finite non-zero bounds and zero center!"
        assert not ((lb == -float('inf') and ub <  float('inf')) \
                or  (ub ==  float('inf') and lb > -float('inf'))), \
            "One bound is finite while the other is infinite!"

        if ub == float('inf') and lb == -float('inf'):
            # In this case, the upper bound is +inf and the lower bound is
            # -inf, which means the interval is R := (-inf, +inf). Here we
            # simply set the center zero because operation `ub + lb`
            # returns nan.
            center = 0
            percent = float('inf')
            return Interval(center, percent)
        elif ub == lb == 0:
            return Interval(0, 0)
        else:
            center = (ub + lb) / 2

        if ub == lb == float('inf') or ub == lb == -float('inf'):
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
        # The center and percent are not allowed to be infty
        # simultaneously.
        if abs(self.center) == float('inf'):
            self.percent = 0

        # Updator will not work normally if the center is zero and the
        # percentage is infinity because the value of width is not
        # computable.
        if self.center == 0 and self.percent == float('inf'):
            self.width = float('inf')
            self.lower_bound = -float('inf')
            self.upper_bound = float('inf')
        elif abs(self.center) == float('inf'):
            self.width = 0
            self.lower_bound = self.center
            self.upper_bound = self.center
        else:
            self.width = abs(self.center) * self.percent / 100.0
            self.lower_bound = self.center - self.width
            self.upper_bound = self.center + self.width
        
    def __add__(self, itv):
        """
        Interval Addition
        """
        new_center = self.center + itv.center
        new_percent = (self.percent * abs(self.center) \
                       + itv.percent * abs(itv.center)) \
                       / (1.0 * abs(self.center + itv.center))
        return Interval(new_center, new_percent)

    def __sub__(self, itv):
        """
        Interval subtraction
        """
        new_center = self.center - itv.center
        new_percent = (self.percent * abs(self.center) \
                       + itv.percent * abs(itv.center)) \
                       / (1.0 * abs(self.center - itv.center))
        return Interval(new_center, new_percent)



    


