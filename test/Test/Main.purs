module Test.Main where

import Prelude

import Data.Array (head, any)
import Data.Array (length) as Array
import Data.Geometry
  ( class Analytic
  , circle
  , dot
  , halfline
  , length
  , line
  , meets
  , middle
  , normalTo
  , point
  , quadratic
  , segment
  , toCoordinates
  , vector
  , wedge
  , (<+|)
  , ellipse
  , ellipseInternal
  , residualConstant
  , moveEllipse
  , Vector(..)
  )
import Data.Tuple.Nested ((/\))
  
import Data.Maybe (fromMaybe)
import Data.Number (abs, sqrt)
import Data.Sparse.Polynomial (Polynomial, (^), (:.))
import Effect (Effect)
import Test.Assert (assert')

class Rougly a where
  roughly :: a -> a -> Boolean
  
instance Rougly Number where
  roughly a b = abs (b - a) < 1e-6
else instance Rougly (Polynomial Number) where
  roughly p q = (sqrt $ ((\x -> x*x) <$> (p - q)) :. 1.0) < 1e-6
else instance
  ( Analytic a
  ) => Rougly a where
  roughly a b = roughly (toCoordinates a) (toCoordinates b)

main :: Effect Unit
main = do
  let
    confidentHead = head >>> fromMaybe (point zero)
    
    a = point $ 310.0^0 + 320.0^1
    b = point $ 100.0^0 + 210.0^1
    c = circle a (length $ vector a b)
    n = normalTo $ vector a b
    d = halfline a n
    e = confidentHead $ d `meets` c
    eb = segment e b
    i = middle eb
    f = confidentHead $ c `meets` (halfline a (vector b a))
    g = confidentHead $ (line a e) `meets` (line i f)
    h = confidentHead $ 
      (halfline b (vector b g)) `meets` (segment e f)
  assert' "elementary computations" $ h `roughly` (point $ 470.0^0 + 270.0^1)
  
  let
    perpendicularBisector p1 p2 = line m1 m2
      where
        m1 = middle $ segment p1 p2
        m2 = m1 <+| normalTo (vector p1 p2)
    roughlyAmong x = any (_ `roughly` x)
    
    j = point $ 2.0^0 + 2.0^1
    k = point $ 3.0^0 + 5.0^1
    l = point $ 4.0^1
    m = point $ -2.0^0 + 2.0^1
    p = point $ -3.0^0 - 1.0^1
    q = confidentHead $
      perpendicularBisector p m `meets` perpendicularBisector m j
    r = point $ (-3.0)^0+(1.0/3.0)^1
    v = Vector $ 10.0^0+10.0^1
    ell = moveEllipse (-v) $ quadratic $ (_ <+| v) <$> [point zero, j, k, l, p]
    intersections = ell `meets` circle q (length $ vector q m)
  assert' "ellipse computations" $ 
    p `roughlyAmong` intersections &&
    j `roughlyAmong` intersections &&
    m `roughlyAmong` intersections &&
    r `roughlyAmong` intersections &&
    Array.length intersections == 4

  let
    s = point $ 2.0^0 + 7.0^1 + 17.0^2
    t = point $ 3.0^0 + 11.0^1 + 19.0^2
    u = point $ 5.0^0 + 13.0^1 + 23.0^2
    w = vector s t `wedge` vector s u
    sw = (vector (point zero) s `dot` w)
  assert' "spatial computations" $
     sw `roughly` (vector (point zero) t `dot` w)
    && sw`roughly` (vector (point zero) u `dot` w)
    
  pure unit
