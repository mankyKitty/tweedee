module Basics where

newtype X = X Double
newtype Y = Y Double
newtype Z = Z Double

data Point = Point (X,Y,Z)
data Vector = Vector (X,Y,Z)

_addTuple :: (X,Y,Z) -> (X,Y,Z) -> (X,Y,Z)
_addTuple (a,b,c) (d,e,f) = (a + b, c + d, e + f)

_subTuple :: (X,Y,Z) -> (X,Y,Z) -> (X,Y,Z)
_subTuple (a,b,c) (d,e,f) = (a - b, c - d, e - f)

_mulTuple :: (X,Y,Z) -> (X,Y,Z) -> (X,Y,Z)
_mulTuple (a,b,c) (d,e,f) = (a * b, c * d, e * f)

addVectorToPoint :: Point -> Vector -> Point
addVectorToPoint (Point p) (Vector v) =
  Point $ _addTuple p v

subtractVectorFromPoint :: Point -> Vector -> Point
subtractVectorFromPoint (Point p) (Vector v) =
  Point $ _subTuple p v

subtractPointFromPoint :: Point -> Point -> Vector
subtractPointFromPoint (Point p) (Vector v) =
  Vector $ _subTuple p v

addVectorToVector :: Vector -> Vector -> Vector
addVectorToVector (Vector v1) (Vector v2) =
  Vector $ _addTuple v1 v2

subtractVectorFromVector :: Vector -> Vector -> Vector
subtractVectorFromVector (Vector v1) (Vector v2) =
  Vector $ _subTuple v1 v2

drawPoint :: Point -> IO ()
drawPoint = print

rotateVectorXY :: Vector -> Degrees -> Vector
rotateVectorXY (Vector (a,b,c)) d =
	Vector (
		(cos d * a) + ((negate $ sin d) * a),
		(sin d * b) + (cos d * b),
		c
	)

rotateVectorXZ :: Vector -> Degrees -> Vector
rotateVectorXZ (Vector (a,b,c)) d =
	Vector (
		(cos d * a) + (sin d * a),
		b,
		((negate $ sin d) * c) + (cos d * c)
	)

rotateVectorYZ :: Vector -> Degrees -> Vector
rotateVectorYZ (Vector (a,b,c)) d =
	Vector (
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
