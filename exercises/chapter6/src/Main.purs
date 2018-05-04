module Main where

import Prelude
import Data.Array
import Data.Eq
import Data.Foldable
import Data.Ordering
import Data.Hashable
import Data.Monoid
import Data.Semigroup
import Data.Show
import Math

-- Complex data
newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

c0 = Complex { real: 0.0, imaginary: 0.0 }
c1 = Complex { real: 1.0, imaginary: 1.0 }
c2 = Complex { real: 2.0, imaginary: 2.0 }


-- NonEmpty data
data NonEmpty a = NonEmpty a (Array a)

n0  = NonEmpty 0 []
n0' = NonEmpty 1 []
n1  = NonEmpty 0 [1]
n1' = NonEmpty 1 [1]
n2  = NonEmpty 0 [1, 2]
n2' = NonEmpty 1 [1, 2]
n3  = NonEmpty 0 [1, 2, 3]
n3' = NonEmpty 1 [1, 2, 3]


-- OneMore data
data OneMore f a = OneMore a (f a)

o1 = OneMore 0 [1]
o2 = OneMore 0 [1,2]
o3 = OneMore 0 [1,2,3]


-- A cleaner definition of showCompare function than in the book.
showCompare :: forall a. Ord a => Show a => a -> a -> String
showCompare a1 a2
  | a1 < a2 = show a1 <> " is less than " <> show a2
  | a1 > a2 = show a1 <> " is greater than " <> show a2
  | otherwise = show a1 <> " is equal to " <> show a2


{-
Define Show and Eq instances for Complex class.
-}

instance showComplex :: Show Complex where
  show (Complex x) =
    (show x.real) <> "+" <> (show x.imaginary) <> "i"
    {- or perhaps more idiomatic:
    "[Real: " <> (show x.real) <>
    ", Imaginary: " <> (show x.imaginary) <> "]"
    -}

instance eqComplex :: Eq Complex where
  eq (Complex x) (Complex y) =
    (x.real == y.real) && (x.imaginary == y.imaginary)

{-
Exercise: Eq instance for the type NonEmpty a
-}

instance showNonEmpty :: Show a =>
  Show (NonEmpty a)
  where
    show :: NonEmpty a -> String
    show (NonEmpty x xs) = (show x) <> ":" <> (show xs)

instance eqNonEmpty :: (Eq a, Eq (Array a)) =>
  Eq (NonEmpty a)
  where
    eq :: NonEmpty a -> NonEmpty a -> Boolean
    eq (NonEmpty x xs) (NonEmpty y ys) = (x == y) && (xs == ys)


{-
Exercise: Semigroup instance for NonEmpty a
-}

instance semigroupNonEmpty :: (Semigroup (Array a)) =>
  Semigroup (NonEmpty a)
  where
    append :: NonEmpty a -> NonEmpty a -> NonEmpty a
    append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)


{-
Exercise: Functor instance for NonEmpty.
-}

instance functorNonEmpty ::
  Functor NonEmpty
  where
    map :: forall a b. (a -> b) -> NonEmpty a -> NonEmpty b
    map f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)


{-
Exercise: Ord instance for Extended a
-}

data Extended a = Finite a | Infinite

instance showExtended :: (Show a) =>
  Show (Extended a)
  where
    show Infinite = "Infinite"
    show (Finite x) = "(Finite " <> show x <> ")"

instance eqExtended :: (Eq a) =>
  Eq (Extended a)
  where
    eq (Finite x) (Finite y) = (x == y)
    eq (Finite _) Infinite   = false
    eq (Infinite) (Finite _) = false
    eq Infinite   Infinite   = false

instance ordExtended :: (Ord a) =>
  Ord (Extended a)
  where
    compare (Finite x) (Finite y) = compare x y
    compare (Finite _) Infinite   = LT
    compare Infinite   (Finite _) = GT
    compare Infinite   Infinite   = EQ

{-
Exercise: Foldable instances for NonEmpty.
-}

instance foldableNonEmpty :: (Monoid (Array a)) =>
  Foldable NonEmpty
  where
    foldr f acc (NonEmpty x xs) = foldr f acc ([x] <> xs)
    foldl f acc (NonEmpty x xs) = foldl f acc ([x] <> xs)
    foldMap f (NonEmpty x xs) = foldMap f ([x] <> xs)


{-
Exercise: foldableOneMore instance.
-}

instance foldableOneMore :: (Foldable f) =>
  Foldable (OneMore f)
  where
    foldr f acc (OneMore x ys) = f x (foldr f acc ys)
    foldl f acc (OneMore x ys) = f (foldl f acc ys) x
    foldMap f (OneMore x ys) =  (f x) <> (foldMap f ys)

instance showOneMore :: (Foldable f, Show a) =>
  Show (OneMore f a)
  where
    show :: OneMore f a -> String
    show (OneMore o (m)) =
      (show o) <> ":" <> foldMap (\x -> show x <> ".") m
