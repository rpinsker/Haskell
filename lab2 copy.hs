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
    Line Double Double
    deriving (Show, Eq)
             
getX (Point a b) = a
getY (Point a b) = b

--Checks if point is above (returns 1), below (returns -1), or on a line (returns 0). 
relativeToLine :: Num a => Point -> Path -> a 
relativeToLine (Point x y) (Line m b) 
  | y > m * x + b = 1
  | y == m * x + b = 0
  | y < m * x + b = -1
                    
--Checks if a Line intersects a LineSegment
intersects :: Path -> LineSegment -> Bool
intersects (Line m b) (LineSegment p1 p2)
  | relativeToLine p1 (Line m b) * relativeToLine p2 (Line m b) == -1 = True
  | relativeToLine p1 (Line m b) == 0 = True
  | relativeToLine p2 (Line m b) == 0 = True                                      
  | otherwise = False                                      
                                 
--Data type definition consisting of Triangle, Quadrilateral, and Circle.
data Shape = Triangle Point Point Point
             | Quadrilateral Point Point Point Point
             | Circle Point Double  
    deriving (Show, Eq)           
             
--Data type definition for a BoundingBox of a shape
data BoundingBox = BoundingBox Point Point
    deriving (Show, Eq)               

--The following functions find the min and max of three and four values
minOfThree :: Ord a => a -> a -> a -> a
minOfThree x y z = min (min x y) z
maxOfThree :: Ord a => a ->a -> a -> a
maxOfThree x y z = max (max x y) z
minOfFour :: Ord a => a ->a -> a -> a -> a
minOfFour w x y z = min (min (min x y) z) w
maxOfFour :: Ord a => a ->a -> a -> a -> a
maxOfFour w x y z = max (max (max x y) z) w

-- Creates a BoundingBox for a certain shape
boundShape :: Shape -> BoundingBox
boundShape (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = BoundingBox (Point (minOfThree x1 x2 x3) (minOfThree y1 y2 y3)) (Point (maxOfThree x1 x2 x3) (maxOfThree y1 y2 y3))
boundShape (Quadrilateral (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)) = BoundingBox (Point (minOfFour x1 x2 x3 x4) (minOfFour y1 y2 y3 y4)) (Point (maxOfFour x1 x2 x3 x4) (maxOfFour y1 y2 y3 y4))
boundShape (Circle (Point x y) r) = BoundingBox (Point (x-r) (y-r)) (Point (x+r) (y+r))

--Checks if a line intersects a bounding box 
intersectsBB :: BoundingBox -> Path -> Bool
intersectsBB (BoundingBox (Point x1 y1) (Point x2 y2)) (Line m b)
  | intersects (Line m b) (LineSegment (Point x1 y1) (Point x2 y1)) = True
  | intersects (Line m b) (LineSegment (Point x1 y1) (Point x1 y2)) = True
  | intersects (Line m b) (LineSegment (Point x1 y2) (Point x2 y2)) = True
  | intersects (Line m b) (LineSegment (Point x2 y1) (Point x2 y2)) = True
  | otherwise = False                                                                   
--Checks if a shape and path intersect                
--mightIntersectShape = Shape -> Path -> Bool                
mightIntersectShape s l = intersectsBB (boundShape s) l 
    
--Checks if a Path intersects either a Quadrilateral or Triangle                          
intersectsShape (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) (Line m b) 
  | intersects (Line m b) (LineSegment (Point x1 y1) (Point x2 y2)) = True
  | intersects (Line m b) (LineSegment (Point x1 y1) (Point x3 y3)) = True
  | intersects (Line m b) (LineSegment (Point x2 y2) (Point y2 y3)) = True
  | otherwise = False                                                              

intersectsShape (Quadrilateral (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)) (Line m b)
  | intersects (Line m b) (LineSegment (Point x1 y1) (Point x2 y2)) = True
  | intersects (Line m b) (LineSegment (Point x2 y2) (Point x3 y3)) = True
  | intersects (Line m b) (LineSegment (Point x3 y3) (Point x4 y4)) = True
  | intersects (Line m b) (LineSegment (Point x4 y4) (Point x1 y1)) = True            
  | otherwise = False                                                                    
                




                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                  
                                                                                                                    