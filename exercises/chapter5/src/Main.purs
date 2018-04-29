module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Maybe
import Data.Picture
import Math as Math


{-
Compute factorial value.
-}
factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 1
factorial n = n * factorial (n - 1)


{-
Compute binomialCoefficient using Pascal's Rule.
-}
binomialCoefficient :: Int -> Int -> Int
binomialCoefficient _ 0 = 1
binomialCoefficient 1 _ = 1
binomialCoefficient n k | k < 0 = 0
                        | k > n = 0
                        | k > n - k = binomialCoefficient n (n - k)
                        | n < 1 = 1
                        | otherwise = (binomialCoefficient (n-1) k)
                                    + (binomialCoefficient (n-1) (k-1))


{-
Evaluate whether two people live in same city.
-}
type Address = { street :: String, city :: String }
showAddress :: Address -> String
showAddress a = a.street <> ", " <> a.city

type Person = { name :: String, address :: Address }
showPerson :: Person -> String
showPerson p = p.name <> ", " <> showAddress p.address

equalAddressCity :: Address -> Address -> Boolean
equalAddressCity a b = a.city == b.city

equalPersonAddressCity :: Person -> Person -> Boolean
equalPersonAddressCity a b = equalAddressCity a.address b.address

sameCity :: Person -> Person -> Boolean
sameCity = equalPersonAddressCity

aliceHome = { street: "123 Alice St", city: "Miami" }
alice = { name: "Alice Smith", address: aliceHome }

bobHome = { street: "123 Bob St", city: "Miami" }
bob = { name:  "Bob Jones", address: bobHome }

carolHome = { street: "123 Carol St", city: "Tampa" }
carol = { name:"Carol Green", address: carolHome }


{-
Extract value from singleton array, if exists,
otherwise return default value (first param).
-}
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [s] = s
fromSingleton d _ = d


{-
Simple Shape: circle centered at origin w/ radius 10.
-}
circleTen :: Shape
circleTen = Circle centre radius
  where
    centre :: Point
    centre = Point { x: 0.0, y: 0.0 }
    radius :: Number
    radius = 10.0


{-
Scale Shape.
-}
scaleX2 :: Shape -> Shape
scaleX2 (Circle centre radius)
        = Circle centre (2.0 * radius)
scaleX2 (Rectangle centre width height)
        = Rectangle centre (2.0 * width) (2.0 * height)
scaleX2 (Line (Point start) (Point end))
        = Line newStart newEnd
          where
            newStart :: Point
            newStart = Point {x: start.x, y: start.y}
            newEnd :: Point
            newEnd = Point {
              x: (start.x + (2.0 * ((end.x + start.x)) / 2.0)),
              y: (start.y + (2.0 * ((end.y + start.y)) / 2.0))
            }
scaleX2 (Text centre text)
        = Text centre text

origin :: Point
origin = Point { x: 0.0, y: 0.0 }

line1 :: Shape
line1 = Line p1 p2
          where
            p1 :: Point
            p1 = Point { x: 1.0, y: 1.0 }
            p2 :: Point
            p2 = Point { x: 2.0, y: 2.0 }

line2 :: Shape
line2 = Line (Point { x: 1.0, y: 1.0 }) (Point { x: 3.0, y: 3.0 })


{-
Extract text from Shape.
-}
shapeText :: Shape -> Maybe String
shapeText (Text centre text)
        = Just text
shapeText _ = Nothing

text1 :: Shape
text1 = Text (Point {x: 1.0, y: 2.0}) "Text1"


{-
Compute area of shape.
-}
area :: Shape -> Number
area (Circle centre radius) = Math.pi * (radius * radius)
area (Rectangle centre width height) = width * height
area (Line p0 p1) = 0.0
area (Text centre text) = 0.0

circle1 :: Shape
circle1 = Circle (Point {x: 1.0, y: 1.0}) 1.0
circle1Area = area circle1

square1 :: Shape
square1 = Rectangle (Point {x: 1.0, y: 1.0}) 1.0 1.0
square1Area = area square1


{-
Extend Shape with Clipped contructor
which clips another Picture to a rectangle.
-}
data Shape'
  = Rectangle' Point Number Number
  | Clipped Shape' Shape

shapeBounds' :: Shape' -> Bounds
shapeBounds' (Rectangle' c w h)
  = shapeBounds (Rectangle c w h)
shapeBounds' (Clipped board shape)
  = union (shapeBounds' board) (shapeBounds shape)

clipboard1 :: Shape'
clipboard1 = Rectangle' (Point {x: 3.0, y: 3.0}) 2.0 2.0

clippedCircle1 :: Shape'
clippedCircle1 = Clipped clipboard1 circle1

clippedCircle1Bounds = shapeBounds' clippedCircle1


{-
Addendum per Chapter 6: Use the showShape function
to define a Show instance for the Shape type.
-- Note:  Unable to directly extend Shape type defined
          defined in another module, so redefined here
          as Shape'' and extended w/ Shape'.
-}

data Shape''
  = Circle'' Point Number
  | Rectangle'' Point Number Number
  | Line'' Point Point
  | Text'' Point String
  | Clipped'' Shape' Shape

instance showShape' :: Show Shape'' where
  show (Rectangle'' c w h)
    = showShape (Rectangle c w h)
  show (Circle'' c r)
    = showShape (Circle c r)
  show (Line'' p0 p1)
    = showShape (Line p0 p1)
  show (Text'' c t)
    = showShape (Text c t)
  show (Clipped'' (Rectangle' c w h) shape)
    = showShape shape <> " CLIPPED ONTO " <> showShape (Rectangle c w h)
  show _ = "Huh?"

circle2 :: Shape''
circle2 = Circle'' (Point {x: 1.0, y: 1.0}) 1.0

clippedCircle2 :: Shape''
clippedCircle2 = Clipped'' clipboard1 circle1
