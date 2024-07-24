module Data.Geometry 
  ( module Data.Geometry.Types
  , module Data.Geometry.Ellipse
  , module Data.Geometry.Space
  ) where

import Data.Geometry.Types 
  ( class Analytic
  , class EuclideanSpace
  , class Intersectable
  , class Metric
  , class Shape
  , Circle(..), HalfLine(..), Line(..), Point(..), Segment(..), Vector(..)
  , circle, halfline, line, point, segment, vector
  , anyPoint2, anyVector2
  , cosAngle, dot, freeVector, fromCoordinates
  , index, length, meets, middle, normalTo, normalized
  , projection, rotated, scale, shape, system, toCoordinates, translatedBy
  , immerse, drain, projector, project
  , (<+|)
  )

import Data.Geometry.Ellipse 
  ( Ellipse(..)
  , cardinal, ellipse, ellipseCenter, ellipseDimensions, ellipseInternal
  , foci, fromUnitCircle, residualConstant
  , rytz, steiner, unCouple
  , quadratic, brianchon, turnEllipse, moveEllipse, expandEllipse
  , mkMatrix
  )

import Data.Geometry.Space
  ( wedge
  , landing
  , land
  , revolution
  )
