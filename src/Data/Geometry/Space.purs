module Data.Geometry.Space where

import Prelude

import Data.Geometry.Types (Vector(..), projector)
import Data.Number (cos, sin, abs)
import Data.Sparse.Matrix  (Matrix(..), eye, transpose, (!!))
import Data.Sparse.Polynomial ((!), (^))

wedge :: Vector 3 -> Vector 3 -> Vector 3
wedge (Vector u) (Vector v) =
  Vector $ 
      (u!1 * v!2 - u!2 * v!1)^0
    + (u!2 * v!0 - u!0 * v!2)^1
    + (u!0 * v!1 - u!1 * v!0)^2

landing :: Vector 3 -> Matrix Number
landing (Vector n)
  | abs (n!2 + 1.0) < 1e-9 = eye 3
  | otherwise = Matrix 
    { height: 3
    , width: 3
    , coefficients:
      (1.0 - a*a / (1.0 + c))^0^0 + (-a*b /(1.0 + c))^0^1 - a^0^2
      + (-a*b/(1.0+c))^1^0 + (1.0 - b*b / (1.0 + c))^1^1 - b^1^2
      + a^2^0 + b^2^1 + c^2^2
    }
    where
      a = n!0
      b = n!1
      c = n!2
      
land :: Vector 3 -> Vector 3 -> Vector 3
land n (Vector u) = Vector $ v.coefficients ! 0
  where Matrix v = landing n * Matrix { height:3, width: 1, coefficients: u^0 }


-- | rotation de u d'un angle a autour de l'axe normalisé n
revolution :: Vector 3 -> Number -> Vector 3 -> Vector 3
revolution n a (Vector u') = Vector $ v.coefficients ! 0
  where 
    u = Matrix { height: 3, width: 1, coefficients: u'^0 }
    p0 = projector n n * u
    l = landing n
    p1 = l * p0
    p2 = Matrix
      { height: 3
      , width: 1
      , coefficients:
       (p1 !! [0,0] * cos a - p1!![1,0] * sin a)^0^0
        + (p1!! [0,0] * sin a + p1!![1,0] * cos a)^1^0
        + (p1!![2,0])^2^0
      }
    p3 = transpose l * p2
    Matrix v = p3 + u - p0
