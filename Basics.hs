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

axiomOneAndTwo :: Point -> Point -> Bool
axiomOneAndTwo p q = let v = p - q in q == p + v
