{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Lib where

import Control.Lens ( Rewrapped, Unwrapped (..), Wrapped (..),makeWrapped, _Wrapped, to, (^.))
import Data.Radian (toRadians)

newtype X =
  X Double
  deriving (Show, Eq, Num, Fractional, Floating)
makeWrapped ''X

newtype Y =
  Y Double
  deriving (Show, Eq, Num, Fractional, Floating)
makeWrapped ''Y

newtype Z =
  Z Double
  deriving (Show, Eq, Num, Fractional, Floating)
makeWrapped ''Z

newtype Degrees =
  Degrees Double
  deriving (Show, Eq, Num, Fractional, Floating)
makeWrapped ''Degrees

data Point =
  Point (X,Y,Z)
  deriving (Show,Eq)

addTuple :: (X,Y,Z) -> (X,Y,Z) -> (X,Y,Z)
addTuple (a,b,c) (d,e,f) = (a + d, b + e, c + f)

subTuple :: (X,Y,Z) -> (X,Y,Z) -> (X,Y,Z)
subTuple (a,b,c) (d,e,f) = (a - d, b - e, c - f)

mulTuple :: (X,Y,Z) -> (X,Y,Z) -> (X,Y,Z)
mulTuple (a,b,c) (d,e,f) = (a * d, b * e, c * f)

instance Num (X,Y,Z) where
  (+) = addTuple
  (-) = subTuple
  (*) = mulTuple

  abs (a,b,c) = (abs a, abs b, abs c)

  signum (a,b,c) = (signum a, signum b, signum c)

  fromInteger n =
    ( X $ fromInteger n
    , Y $ fromInteger n
    , Z $ fromInteger n
    )

data Vector = Vector (X,Y,Z)
  deriving (Show,Eq)

data Scale = Scale (X,Y,Z)
  deriving (Show,Eq)

runFMul
  :: forall a b.
     ( Floating (Unwrapped a)
     , Floating (Unwrapped b)
     , Rewrapped a a
     , Rewrapped b b
     )
  => (Unwrapped a -> Unwrapped b)
  -> a
  -> b
  -> Unwrapped b
runFMul g d b = b ^. _Wrapped . to (* g r)
  where r = d ^. _Wrapped . toRadians

addVectorToPoint :: Point -> Vector -> Point
addVectorToPoint (Point p) (Vector v) = Point (p + v)

subtractVectorFromPoint :: Point -> Vector -> Point
subtractVectorFromPoint (Point p) (Vector v) = Point (p - v)

subtractPointFromPoint :: Point -> Point -> Vector
subtractPointFromPoint (Point p) (Point p') = Vector (p - p')

addVectorToVector :: Vector -> Vector -> Vector
addVectorToVector (Vector v1) (Vector v2) = Vector (v1 + v2)

subtractVectorFromVector :: Vector -> Vector -> Vector
subtractVectorFromVector (Vector v1) (Vector v2) = Vector (v1 + v2)

drawPoint :: Point -> IO ()
drawPoint = print

rotateVectorXY :: Vector -> Degrees -> Vector
rotateVectorXY (Vector (a,b,c)) d =
  Vector
    ( X $ (runFMul cos d a) + (runFMul (negate . sin) d a)
    , Y $ (runFMul sin d b) + (runFMul cos d b)
    , c
    )

rotateVectorXZ :: Vector -> Degrees -> Vector
rotateVectorXZ (Vector (a,b,c)) d =
  Vector
    ( X $ (runFMul cos d a) + (runFMul sin d a)
    , b
    , Z $ (runFMul (negate . sin) d c) + (runFMul cos d c)
    )

rotateVectorYZ :: Vector -> Degrees -> Vector
rotateVectorYZ (Vector (a,b,c)) d =
  Vector
    ( a
    , Y $ (runFMul cos d b) + (runFMul (negate . sin) d b)
    , Z $ (runFMul sin d c) + (runFMul cos d c)
    )

scalePoint :: Point -> Scale -> Point
scalePoint (Point p) (Scale s) = Point (p * s)

axiomOneAndTwo :: Point -> Point -> Bool
axiomOneAndTwo (Point p) (Point q) = q == p + (p - q)
