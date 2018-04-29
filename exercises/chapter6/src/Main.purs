module Main where

import Prelude
import Data.Eq
import Data.Ordering
import Data.Hashable
import Math


{-
Define Show and Eq instances for Complex class.
-}

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  -- show (Complex x) = (show x.real) <> "+" <> (show x.imaginary) <> "i"
  -- Perhaps more idiomatic?:
  show (Complex x) = "[Real: " <> (show x.real) <>
                     ", Imaginary: " <> (show x.imaginary) <> "]"

instance eqComplex :: Eq Complex where
  eq (Complex x) (Complex y) =
    (x.real == y.real) && (x.imaginary == y.imaginary)

c0 = Complex { real: 0.0, imaginary: 0.0 }
c1 = Complex { real: 1.0, imaginary: 1.0 }
c1' = Complex { real: 2.0, imaginary: 2.0 }
