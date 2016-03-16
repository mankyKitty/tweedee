module Basics where

newtype X = X Double
newtype Y = Y Double
newtype Z = Z Double

data Point = Point (X,Y,Z)
data Vector = Vector (X,Y,Z)

addVectorToPoint :: Point -> Vector -> Point
addVectorToPoint (Point (a,b,c)) (Vector (d,e,f)) =
  Point (a + d, b + e, c + f)

subtractVectorFromPoint :: Point -> Vector -> Point
subtractVectorFromPoint (Point (a,b,c)) (Vector (d,e,f)) =
  Point (a - d, b - e, c - f)

subtractPointFromPoint :: Point -> Point -> Vector
subtractPointFromPoint (Point (a,b,c)) (Vector (d,e,f)) =
  Vector (a - d, b - e, c - f)

addVectorToVector :: Vector -> Vector -> Vector
addVectorToVector (Vector (a,b,c)) (Vector (d,e,f)) =
  Vector (a + d, b + e, c + f)

subtractVectorFromVector :: Vector -> Vector -> Vector
subtractVectorFromVector (Vector (a,b,c)) (Vector (d,e,f)) =
  Vector (a - d, b - e, c - f)

drawPoint :: Point -> IO ()
drawPoint = print

rotatePointXY :: Point -> Degrees -> Point
rotatePointXY (Point (a,b,c)) d =
	Point (
		(cos d * a) + ((negate $ sin d) * a),
		(sin d * b) + (cos d * b),
		c
	)

rotatePointXZ :: Point -> Degrees -> Point
rotatePointXZ (Point (a,b,c)) d =
	Point (
		(cos d * a) + (sin d * a),
		b,
		((negate $ sin d) * c) + (cos d * c)
	)

rotatePointYZ :: Point -> Degrees -> Point
rotatePointYZ (Point (a,b,c)) d =
	Point (
		a,
	  	(cos d * b) + ((negate $ sin d) * b),
	  	(sin d * c) + (cos d * c)
	)

scalePoint :: Point -> Scale -> Point
scalePoint (Point (a,b,c)) (Scale (x,y,z)) =
	Point (a * x, b * y, c * z)

main :: IO ()
main = do
	let p1 = Point (X 1.0, Y 2.0, Z 1.0)
	let p2 = Point (X 0.0, Y 4.0, Z 4.0)
	let v1 = Vector (X 2.0, Y 0.0, Z 0.0)
	
    drawPoint p1 -- should display (1,2,1)
    drawPoint p2 -- should display (0,4,4)

	let v2 = subtractPointFromPoint p1 p2
	let v1' = addVectorToVector v1 v2
	
	let p1' = addVectorToPoint p1 v1'
    drawPoint p1' -- should display (4,0,-2)

	let p2' = subtractVectorFromPoint p2 v2
    drawPoint p2' -- should display (-1,6,7)

axiomOneAndTwo :: Point -> Point -> Bool
axiomOneAndTwo p q = let v = p - q in q == p + v
