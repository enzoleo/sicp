## The solution of exercise 2.2
## Consider the problem of representing line segments in a plane. Each
## segment is represented as a pair of points: a starting point and an
## ending point. Define a constructor `make-segment` and selectors `start-
## segment` and `end-segment` that define the representation of segments
## in terms of points. Furthermore, a point can be represented as a pair
## of numbers: the x coordinate and the y coordinate. Accordingly, specify
## a constructor make-point and selectors `x-point` and `y-point` that
## define this representation.
##
## Finally, using your selectors and constructors, define a procedure
## `midpoint-segment` that takes a line segment as argument and returns
## its midpoint (the point whose coordinates are the average of the
## coordinates of the endpoints). To try your procedures, you'll need a
## way to print points.
##
## -------- (above from SICP)
##

import sys
import math

# Check the python version
# Write different code for different python version
if sys.version_info[0] < 3:
    # Use new type class
    __metaclass__ = type

# Define a new 2D points class over R x R set (Euclidean plane).
class Point2d:
    """This class implements 2d points over R x R set.

    A 2d point is determined by its two coordinates x and y. Of course you
    can construct a 2d point from its polar coordinates.
    """

    __slots__ = ('__x', '__y')

    def __init__(self, x = 0, y = 0):
        """Initialize a 2d point with its x and y coordinates.
        The default is the origin O(0, 0).
        Notice that the coordinates will be converted to floats.
        """
        self.__x = float(x)
        self.__y = float(y)

    @classmethod
    def from_polar(self, radius, angle, pole = (0, 0)):
        """Initialize a 2d point with polar radius and polar angle in the
        polar coordinates system. The default pole is the origin.
        """
        x = radius * math.cos(angle) + pole[0]
        y = radius * math.sin(angle) + pole[1]
        return point_2d(x, y)

    @property
    def x(point):
        return point.__x

    @x.setter
    def x(point, new_x):
        point.__x = float(new_x)

    @property
    def y(point):
        return point.__y

    @y.setter
    def y(point, new_y):
        point.__y = float(new_y)

    def __repr__(self):
        """Print method"""
        return '(%s, %s)' % (self.__x, self.__y)

# Define segment class
class Segment:
    """This class implements segments over R x R set.
    A segment is determined by its start point and end point.
    """

    __slots__ = ('__start', '__end')

    def __init__(self, start, end):
        """Initialize a segment with its start point and end point.
        Here parameter start and end must be type `Point_2d`
        """
        if not (isinstance(start, Point2d) and \
                isinstance(end, Point2d)):
            raise TypeError("cannot initialize segment with endpoints "
                            "in wrong type: %s, %s." % \
                            (type(start).__name__, type(end).__name__))
        self.__start = start
        self.__end   = end

    @property
    def start(segment):
        return segment.__start

    @property
    def end(segment):
        return segment.__end

    def __repr__(self):
        """Print method"""
        return '(%s, %s) -- (%s, %s)' % \
            (self.__start.x, self.__start.y, self.__end.x, self.__end.y)

    def midpoint(self):
        """Compute the midpoint of this segment."""
        return Point2d((self.start.x + self.end.x) / 2,
                       (self.start.y + self.end.y) / 2)

    
    
