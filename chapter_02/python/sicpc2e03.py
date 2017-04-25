## The solution of exercise 2.3
## Implement a representation for rectangles in a plane. (Hint: You may
## want to make use of exercise 2.2.) In terms of your constructors and
## selectors, create procedures that compute the perimeter and the area of
## a given rectangle. Now implement a different representation for
## rectangles. Can you design your system with suitable abstraction
## barriers, so that the same perimeter and area procedures will work
## using either representation? 
##
## -------- (above from SICP)
##

import sys
import math
import copy

# Check the python version
# Write different code for different python version
if sys.version_info[0] < 3:
    # Use new type class
    __metaclass__ = type

# Define a new simple 2D vector class.
class Vec2d:
    """This class implements 2d vectors over R x R set.
    A 2d vector is determined by its two coordinates x and y.
    """

    __slots__ = ('__x', '__y')

    def __init__(self, x = 0, y = 0):
        """Initialize a 2d vector with its x and y elements.
        The default is the zero vector O(0, 0).
        Notice that the elements will be converted to floats.
        """
        self.__x = float(x)
        self.__y = float(y)

    @property
    def x(vec):
        return vec.__x

    @x.setter
    def x(vec, new_x):
        vec.__x = float(new_x)

    @property
    def y(vec):
        return vec.__y

    @y.setter
    def y(vec, new_y):
        vec.__y = float(new_y)

    def __repr__(self):
        """Print method"""
        return '-> (%s, %s)' % (self.__x, self.__y)

    def __add__(self, addend):
        """Addition of vector."""
        if isinstance(addend, Vec2d):
            return Vec2d(self.x + addend.x, self.y + addend.y)
        return NotImplemented

    def __sub__(self, minuend):
        """Subtraction of vector."""
        if isinstance(minuend, Vec2d):
            return Vec2d(self.x - minuend.x, self.y - minuend.y)
        return NotImplemented

    def __mul__(self, multiplier):
        """Multiplication of vector. Only two kinds of multiplication are
        implemented here:
        
          - Multiplication of two vectors is dot production.

          - Multiplication of a vector and a real number is scalar
            multiplication.
        """
        if isinstance(multiplier, Vec2d):
            return self.x * multiplier.x + self.y * multiplier.y
        elif isinstance(multiplier, (int, float)):
            return Vec2d(self.x * multiplier, self.y * multiplier)
        return NotImplemented

    def __rmul__(self, factor):
        """Right multiplication.
        Only float number is accepted to be the factor.
        """
        if isinstance(factor, (int, float)):
            return Vec2d(self.x * multiplier, self.y * multiplier)
        return NotImplemented

    @property
    def length(self):
        """The length of vector"""
        if self.x == 0:
            return abs(self.y)
        elif self.y == 0:
            return abs(self.x)
        else:
            return math.sqrt(self.x ** 2 + self.y ** 2)

    def __copy__(self):
        """Copy this vector."""
        return self.__class__(self.__x, self.__y)
    
    def __deepcopy__(self, memo):
        """Deepcopy this vector."""
        return self.__class__(self.__x, self.__y)

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

    def __add__(self, vec):
        """Add a vector to a point."""
        if isinstance(vec, Vec2d):
            return Point2d(self.x + vec.x, self.y + vec.y)
        return NotImplemented

    def __sub__(self, minuend):
        """Subtract a vector or a point."""
        if isinstance(minuend, Vec2d):
            return Point2d(self.x - minuend.x, self.y - minuend.y)
        elif isinstance(minuend, Point2d):
            return Vec2d(self.x - minuend.x, self.y - minuend.y)
        return NotImplemented

    def __copy__(self):
        """Copy this point"""
        return self.__class__(self.__x, self.__y)
    
    def __deepcopy__(self, memo):
        """Deepcopy this point."""
        return self.__class__(self.__x, self.__y)

# Define segment class
class Segment:
    """This class implements segments over R x R set.
    A segment is determined by its start point and end point.
    """

    __slots__ = ('__start', '__end')

    def __init__(self, start, end):
        """Initialize a segment with its start point and end point.
        Here parameter start and end must be type `Point_2d`.

        Notice that the type of start and end are both Point_2d, here the
        assignment means the segment is binded with the input arguments.
        For example, there exists two points:

            p1 = Point2d(5, 7)
            p2 = Point2d(3, 9)

        And the segment is constructed by:
        
            s = Segment(p1, p2)

        Then the start point of s is binded with p1 and the end point of s
        is binded with p2, which means, if you change the coordinates of
        p1 or p2, the segment s will also changes. To prevent binding, you
        can construct the segment by:

            import copy
            s = Segment(copy(p1), copy(p2))

        And use property to change the segment.
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

    @start.setter
    def start(segment, new_start):
        if not isinstance(new_start, Point2d):
            raise TypeError("cannot set attribute with endpoints "
                            "in wrong type: %s." % \
                            type(new_start).__name__)
        segment.__start = new_start

    @property
    def end(segment):
        return segment.__end

    @end.setter
    def end(segment, new_end):
        if not isinstance(new_end, Point2d):
            raise TypeError("cannot set attribute with endpoints "
                            "in wrong type: %s." % \
                            type(new_end).__name__)
        segment.__end = new_end

    @property
    def length(self):
        vec = self.end - self.start
        return vec.length

    def __repr__(self):
        """Print method"""
        return '(%s, %s) -- (%s, %s)' % \
            (self.start.x, self.start.y, self.end.x, self.end.y)

    def midpoint(self):
        """Compute the midpoint of this segment."""
        return Point2d((self.start.x + self.end.x) / 2,
                       (self.start.y + self.end.y) / 2)

    def __copy__(self):
        """Copy this segment"""
        return self.__class__(self.__start, self.__end)
    
    def __deepcopy__(self, memo):
        """Deepcopy this segment."""
        return self.__class__(copy.copy(self.__start), \
                              copy.copy(self.__end))

# Define parallelogram class
class Paral:
    """This class implements 2d parallelogram.

    A parallelogram is determined by its basepoint (one of four vertices),
    and its two edges, denoted by two vectors.
    """

    __slots__ = ('__bp', '__eva', '__evb')

    def __init__(self, bp, eva, evb):
        """Initialize a parallelogram with a basepoint and two edges.
        The type of basepoint must be Point2d, and the type of two edges
        must be Vec2d
        """
        if not isinstance(bp, Point2d):
            raise TypeError("cannot initialize parallelogram because of "
                            "wrong basepoint type: %s" % \
                            type(bp).__name__)
        if not (isinstance(eva, Vec2d) and \
                isinstance(evb, Vec2d)):
            raise TypeError("cannot initialize parallelogram because of "
                            "wrong edge type: %s, %s" % \
                            (type(eva).__name__, type(evb).__name__))

        self.__bp = bp
        self.__eva, self.__evb = eva, evb

    @property
    def area(self):
        """The area of this parallelogram."""
        return abs(self.eva.x * self.evb.y - self.eva.y - self.evb.x)

    @property
    def perimeter(self):
        """The perimeter of this parallelogram."""
        return 2 * (self.eva.length + self.evb.length)

    @property
    def bp(self):
        return self.__bp

    @bp.setter
    def bp(paral, new_bp):
        if not isinstance(new_bp, Point2d):
            raise TypeError("cannot set attribute because of "
                            "wrong basepoint type: %s" % \
                            type(new_bp).__name__)
        paral.__bp = new_bp

    @property
    def eva(self):
        return self.__eva

    @eva.setter
    def eva(paral, new_eva):
        if not isinstance(new_eva, Vec2d):
            raise TypeError("cannot initialize parallelogram because of "
                            "wrong edge type: %s" % \
                            type(new_eva).__name__)
        paral.__eva = new_eva

    @property
    def evb(self):
        return self.__evb

    @evb.setter
    def evb(paral, new_evb):
        if not isinstance(new_evb, Vec2d):
            raise TypeError("cannot initialize parallelogram because of "
                            "wrong edge type: %s" % \
                            type(new_evb).__name__)
        paral.__evb = new_evb

    def __repr__(self):
        """Print method"""
        return '%r -- %r -- %r -- %r' % \
            (self.bp, self.bp + self.eva, \
             self.bp + self.eva + self.evb, \
             self.bp + self.evb)


    
