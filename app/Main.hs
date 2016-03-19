module Main where

import Lib

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
