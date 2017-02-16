-- Avoid to import sort
import Data.List hiding (sort)

-- Avoid shallow function name
import qualified Data.Map as Map
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- AVOID: With explicit recursion
findKeyWithRecursion :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyWithRecursion _ [] = Nothing
findKeyWithRecursion key ((k,v):xs)
  | key == k  = Just v
  | otherwise = findKeyWithRecursion key xs

findKeyWithFold :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyWithFold key = foldl (\acc (k,v) -> if k == key then Just v else acc) Nothing

phoneBook =
  [
    ("betty","555-2938"),
    ("bonnie","452-2928"),
    ("patsy","493-2928"),
    ("lucille","205-2928"),
    ("wendy","939-8282"),
    ("penny","853-2492")
  ]
