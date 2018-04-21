module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.MonadZero (guard)
import Data.Array (filter, last, null, (:))
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)

import Data.Path (Path, filename, isDirectory, ls, root, size)
import FileOperations (allFiles)

main :: Eff (console :: CONSOLE) Unit
main = for_ (allFiles root) logShow

infix 8 filter as <$?>

count :: forall a. Array a -> Int
count = count' 0
  where
    count' acc [] = acc
    count' acc xs = count' (acc + 1) (unsafePartial tail xs)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

onlyFiles :: Path -> Array Path
onlyFiles file = filter (\x -> not isDirectory x) $ allFiles file

isLarger :: Path -> Path -> Boolean
isLarger x y = (size x) > (size y)

isSmaller :: Path -> Path -> Boolean
isSmaller x y = (size x) < (size y)

minOrMaxFile :: (Path -> Path -> Boolean) -> Path -> Path
minOrMaxFile cmp dir = foldl minmax firstFile restFiles
  where
    firstFile = unsafePartial head $ onlyFiles $ dir
    restFiles = unsafePartial tail $ onlyFiles $ dir
    minmax acc x = if cmp x acc then x else acc

largestFile :: String
largestFile = filename $ minOrMaxFile isLarger $ root

smallestFile :: String
smallestFile = filename $ minOrMaxFile isSmaller $ root

inFiles :: String -> Array Path -> Boolean
inFiles name files = not null $ filter (\fn -> filename fn == name) $ files

isChild :: String -> Path -> Boolean
isChild name dir = inFiles name $ ls dir

whereIs :: String -> Maybe Path
whereIs name = last $ unsafePartial tail $ whereIs' name root
  where
    whereIs' :: String -> Path -> Array Path
    whereIs' nom file = file : do
      child <- ls file
      guard $ isChild nom child
      whereIs' nom child
