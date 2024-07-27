module Data.Geometry.Types where

import Prelude

import Data.Array (filter)
import Data.Int (fromStringAs, decimal)
import Data.Map (intersectionWith)
import Data.Maybe (fromMaybe)
import Data.Number (sqrt, cos, sin)
import Data.Sparse.Polynomial (Polynomial(..), (:.), (!), (^))
import Data.Sparse.Matrix (Matrix(..), eye, transpose, (!!))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Prim.Int (class ToString, class Add)
import Type.Proxy (Proxy(..))

class Reveal :: Int -> Constraint
class Reveal n where
  revealImpl :: Proxy n -> Int

instance (ToString n s, IsSymbol s) => Reveal n where
  revealImpl _ = 
   fromMaybe 0
      $ fromStringAs decimal $ reflectSymbol (Proxy :: Proxy s)

reveal :: forall @n. Reveal n => Int
reveal = revealImpl @n Proxy

newtype Point (n :: Int) = Point (Polynomial Number)
newtype Vector (n :: Int) = Vector (Polynomial Number)

newtype Segment n = Segment (Point n /\ Point n)
newtype HalfLine n = HalfLine (Point n /\ Vector n)
newtype Line n = Line (Point n /\ Point n)
newtype Circle = Circle (Point 2 /\ Number)

derive newtype instance Show (Point n)
derive newtype instance Show (Vector n)

derive newtype instance Semiring (Vector n)
derive newtype instance Ring (Vector n)

derive newtype instance Show (Segment n)
derive newtype instance Show (HalfLine n)
derive newtype instance Show (Line n)
derive newtype instance Show Circle

segment :: forall n. Point n -> Point n -> Segment n
segment p1 p2 = Segment $ p1 /\ p2

halfline :: forall n. Point n -> Vector n -> HalfLine n
halfline p v = HalfLine $ p /\ v

line :: forall n. Point n -> Point n -> Line n
line p1 p2 = Line $ p1 /\ p2

circle :: Point 2 -> Number -> Circle
circle p r = Circle $ p /\ r

class Analytic a where
  fromCoordinates :: Polynomial Number -> a
  toCoordinates :: a -> Polynomial Number
  index :: a -> Int -> Number 

instance Analytic (Point n) where
  fromCoordinates = Point
  toCoordinates (Point p) = p
  index (Point p) i = p ! i

instance Analytic (Vector n) where
  fromCoordinates = Vector
  toCoordinates (Vector v) = v
  index (Vector p) i = p ! i

-- |
-- | Increments the dimension of a point/vector by adding a zero coordinate
-- | after the other coordinates.
-- |
immerse 
  :: forall a n n1
    . Analytic (a n) 
    => Analytic (a n1) => Add n 1 n1 
    => a n -> a n1
immerse = fromCoordinates <<< toCoordinates

-- |
-- | Decrement the dimension of a point/vector by removing its last
-- | coordinate.
-- |
drain :: forall a s n n1
  . ToString n1 s 
  => IsSymbol s 
  => Analytic (a n) => Analytic (a n1) 
  => Add n1 1 n 
  => a n -> a n1
drain x = fromCoordinates $ p - (p ! r) ^ r 
  where 
    p = toCoordinates x
    r = reveal @n1

-- |
-- | Matrix used by the `project` function.
-- |
projector 
  :: forall n s. ToString n s => IsSymbol s 
    => Vector n -> Vector n -> Matrix Number
projector (Vector n) (Vector d) =
  eye r - 
    ( 
      (_ / ((transpose md * mn) !! [0,0])) <$> (md * transpose mn)
    )
  where 
    r = reveal @n
    mn =  Matrix { height: r, width: 1, coefficients: n^0 }
    md =  Matrix { height: r, width: 1, coefficients: d^0 }

-- |
-- | `project n d u` projects a vector `u` on a plane, passing through 
-- | the origin and of normal vector `n`, using a direction parallel to `d`.
-- |
project 
  :: forall n s. ToString n s => IsSymbol s 
    => Vector n -> Vector n -> Vector n -> Vector n
project n d (Vector u) = Vector $ v.coefficients ! 0
  where 
    Matrix v = 
      projector n d 
        * Matrix 
            { height: reveal @n
            , width: 1
            , coefficients: u^0 
            }

class Shape :: Int -> (Int -> Type) -> Constraint
class Shape n s where
  shape :: Proxy n -> Polynomial Number -> s n

instance Analytic (s n) => Shape n s where
  shape _ = fromCoordinates 

point :: forall @n. Shape n Point => Polynomial Number -> Point n
point = shape @n Proxy

freeVector :: forall @n. Shape n Vector => Polynomial Number -> Vector n
freeVector = shape @n Proxy

vector :: forall n. Point n -> Point n -> Vector n
vector (Point a) (Point b) = Vector (b - a)

translatedBy :: forall n. Point n -> Vector n -> Point n
translatedBy (Point a) (Vector v) = Point (a + v)

infixl 6 translatedBy as <+|

middle :: forall n. Segment n -> Point n
middle (Segment (Point a /\ Point b)) = Point $ 0.5^0 * (a + b)

scale :: forall n. Number -> Vector n -> Vector n
scale k (Vector v) = Vector $ k^0 * v

class EuclideanSpace a where
  dot :: a -> a -> Number
  
class Metric a where
  length :: a -> Number

instance EuclideanSpace (Vector n) where
  dot (Vector (Poly p1)) (Vector (Poly p2)) 
    = (Poly $ intersectionWith (*) p1 p2) :. 1.0
 
instance Metric (Vector n) where
  length v = sqrt $ v `dot` v
 
instance Metric (Segment n) where
  length (Segment (p1 /\ p2)) = length $ vector p1 p2

normalized :: forall n. Vector n -> Vector n
normalized v = scale (1.0 / length v) v

normalTo :: Vector 2 -> Vector 2
normalTo v = Vector $ (- index v 1)^0 + (index v 0)^1

rotated :: Number -> Vector 2 -> Vector 2
rotated ang v =
  Vector $ (index v 0 * cos ang - index v 1 * sin ang)^0 +
           (index v 0 * sin ang + index v 1 * cos ang)^1

-- |
-- | `projection d v` projects a vector `v` on a vector `d`.
-- |
projection :: forall n. Vector n -> Vector n -> Vector n
projection direction v =
  scale ((v `dot` direction) / (direction `dot` direction)) direction

cosAngle :: forall n. Vector n -> Vector n -> Number
cosAngle u v = (u `dot` v) / (length u * length v)

type System = Polynomial (Polynomial Number)

system :: Line 2 -> System
system (Line (m /\ n)) =
  (index m 1 - index n 1)^0^0 +
  (index n 0 - index m 0)^1^0 +
  (index m 0 * index n 1 - index m 1 * index n 0)^2^0

anyPoint2 :: System -> Point 2
anyPoint2 s = 
  Point $ 
    (-s!0!0 * s!0!2 / (s!0!0 * s!0!0 + s!0!1 * s!0!1))^0
    + (-s!0!1 * s!0!2 / (s!0!0 * s!0!0 + s!0!1 * s!0!1))^1

anyVector2 :: System -> Vector 2
anyVector2 s =
  Vector $ (-s!0!1)^0 + (s!0!0)^1

class Intersectable a b where
  meets :: a -> b -> Array (Point 2)

instance Intersectable (Line 2) (Line 2) where
  meets l l' = next
      where
      s = system l ! 0
      s' = system l' ! 0
      delta = s!0 * s'!1 - s'!0 * s!1
      next
        | delta == 0.0 = []
        | otherwise    =
              [ point $ 
                ((s!1 * s'!2 - s'!1 * s!2) / delta)^0 
                  + ((s'!0 * s!2 - s!0 * s'!2) / delta)^1 
              ]

instance Intersectable (Line 2) (HalfLine 2) where
  meets l (HalfLine (origin /\ direction)) =
    filter (\p -> cosAngle (vector origin p) direction >= 0.0) $ l `meets` l'
    where 
      l' = Line $ origin /\ (origin <+| direction)

instance Intersectable (HalfLine 2) (Line 2) where
  meets hl l = meets l hl

instance Intersectable (Line 2) Circle where
  meets (Line (m /\ n)) (Circle (center /\ radius)) =
    next
        where
        u = vector m n
        p = m <+| projection u (vector m center)
        ob = length $ vector center p
        next | ob > radius  = []
             | ob == radius = [p]
             | otherwise    =
                let om = sqrt $ radius * radius - ob *ob
                    v = scale (om / length u) u
                 in [ p <+| v, p <+| (scale (-1.0) v)]

instance Intersectable Circle (Line 2) where
  meets c l = meets l c

instance Intersectable (HalfLine 2) Circle where
  meets (HalfLine (origin /\ direction)) c =
    filter (\p -> cosAngle (vector origin p) direction >= 0.0) $ c `meets` l'
    where 
      l' = Line $ origin /\ (origin <+| direction)

instance Intersectable Circle (HalfLine 2) where
  meets c hl = meets hl c

instance Intersectable Circle Circle where
  meets (Circle (c0 /\ _r0)) c@(Circle (c1 /\ _r1)) = c `meets` l
    where
      x0 = index c0 0
      y0 = index c0 1
      x1 = index c1 0
      y1 = index c1 1
      sys = 
        (2.0 * (x0-x1))^0^0
        + (2.0 * (y0-y1))^1^0
        + (x1 * x1 - x0 * x0
          + y1 * y1 - y0 * y0)^2^0
      p = anyPoint2 sys
      l = Line $ p /\ (p <+| anyVector2 sys)
      

instance Intersectable (HalfLine 2) (HalfLine 2) where
  meets (HalfLine (origin /\ direction)) hl =
    filter (\p -> cosAngle (vector origin p) direction >= 0.0) $
         hl `meets` l
    where 
      l = Line $ origin /\ (origin <+| direction)

instance Intersectable (Segment 2) (Line 2) where
  meets (Segment (origin /\ extremity)) l =
    filter (\p ->
        cosAngle (vector extremity p)
                   (vector extremity origin) >= 0.0) $ hl `meets` l
    where 
      hl = HalfLine $ origin /\ (vector origin extremity)

instance Intersectable (Line 2) (Segment 2) where
  meets l s = meets s l

instance Intersectable (Segment 2) (HalfLine 2) where
  meets s (HalfLine (origin /\ direction)) =
    filter (\p ->
        cosAngle (vector origin p)
                   direction >= 0.0) $ s `meets` l
    where
      l = Line $ origin /\ (origin <+| direction)

instance Intersectable (HalfLine 2) (Segment 2) where
  meets hl s = meets s hl

instance Intersectable (Segment 2) Circle where
  meets (Segment (origin /\ extremity)) c =
    filter (\p ->
        cosAngle (vector extremity p)
                   (vector extremity origin) >= 0.0) $ hl `meets` c
    where
      hl = HalfLine $ origin /\ (vector origin extremity)

instance Intersectable Circle (Segment 2) where
  meets c s = meets s c

instance Intersectable (Segment 2) (Segment 2) where
  meets (Segment (origin /\ extremity)) s =
    filter (\p ->
        cosAngle (vector extremity p)
                   (vector extremity origin) >= 0.0) $ hl `meets` s
    where
      hl = HalfLine $ origin /\ (vector origin extremity)
