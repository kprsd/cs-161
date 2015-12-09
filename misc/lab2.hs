-- A point is a point in the xy plane, represented by x and y coordinates
-- E.g. (Point 0.0 0.0) is the origin, (Point (-1) (1)) is in the top left
-- quadrant.
data Point = Point Double Double
    deriving (Show, Eq)

-- A line segment is a straight line of finite length, defined by its
-- two end points.   E.g. (LineSegment (Point 0 0) (Point 1 1)) is a
-- line segment from the origin to the coordinate (1, 1)
data LineSegment = LineSegment Point Point 
    deriving (Show, Eq)


-- A Path is a 2D path in the xy-plane.  The idea is that Path can be 
-- extended to support straight lines, curves, and arbitrary paths, 
-- but currently there is only one data constructor for Path: Line.
data Path = 
-- Line represents an infinite straight line defined by its slope a
-- and its y intercept b, ie. by the equation y = ax + b
    Line Double Double | Parabola Double Double Double
    deriving (Show, Eq)
    
data Shape = Triangle Point Point Point | Quadrilateral Point Point Point Point | Circle Point Double
    deriving (Show, Eq)

data BoundingBox = BoundingBox Point Point
    deriving (Show, Eq)
    
boundShape (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = BoundingBox (Point (minimum [x1, x2, x3]) (minimum [y1, y2, y3])) (Point (maximum [x1, x2, x3]) (maximum [y1, y2, y3]))
boundShape (Quadrilateral (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)) = BoundingBox (Point (minimum [x1, x2, x3, x4]) (minimum [y1, y2, y3, y4])) (Point (maximum [x1, x2, x3, x4]) (maximum [y1, y2, y3, y4]))
boundShape (Circle (Point x y) r) = BoundingBox (Point (x - r) (y - r)) (Point (x + r) (y + r))

mightIntersectShape p s = intersectsBB p (boundShape s)

intersectsBB p (BoundingBox (Point x1 y1) (Point x2 y2)) = (intersects (LineSegment (Point x1 y1) (Point x1 y2)) p) || (intersects (LineSegment (Point x2 y1) (Point x2 y2)) p) || (intersects (LineSegment (Point x1 y1) (Point x2 y1)) p) || (intersects (LineSegment (Point x1 y2) (Point x2 y2)) p)

segSlope (LineSegment (Point x1 y1) (Point x2 y2)) = (y2 - y1) / (x2 - x1)
segInt (LineSegment (Point x1 y1) (Point x2 y2)) = y2 - slope * x2
    where slope = segSlope (LineSegment (Point x1 y1) (Point x2 y2))

intersects (LineSegment (Point x1 y1) (Point x2 y2)) (Line m b) 
    | (m == segSlope (LineSegment (Point x1 y1) (Point x2 y2))) && (b == segInt (LineSegment (Point x1 y1) (Point x2 y2))) = True
    | (m == segSlope (LineSegment (Point x1 y1) (Point x2 y2))) = False
    | (x1 * m + b <= y1) && (x2 * m + b >= y2) = True
    | (x1 * m + b >= y1) && (x2 * m + b <= y2) = True
    | otherwise = False
    
intersects (LineSegment (Point x1 y1) (Point x2 y2)) (Parabola a b c)
    | ((x1 ** 2) * a + x1 * b + c <= y1) && ((x2 ** 2) * a + x2 * b + c >= y2) = True
    | ((x1 ** 2) * a + x1 * b + c >= y1) && ((x2 ** 2) * a + x2 * b + c <= y2) = True
    | otherwise = False