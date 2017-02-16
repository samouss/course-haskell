comparisonWith100 :: Int -> Ordering
comparisonWith100 = compare 100

divideBy10 :: (Floating a) => a -> a
divideBy10 = (/10)

divide200ByX :: (Floating a) => a -> a
divide200ByX = (200/)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- With guards
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : filter' f xs
  | otherwise = filter' f xs

-- With case of
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' f (x:xs) = case f x of
  True      -> x : filter'' f xs
  otherwise -> filter'' f xs

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x    = x : chain (x `div` 2)
  | otherwise = x : chain ((x * 3) + 1)

-- whithout composition
oddSquareSumWhithoutComposition :: Integer
oddSquareSumWhithoutComposition = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- with composition
oddSquareSumComposition :: Integer
oddSquareSumComposition = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- more readable
oddSquareSumReadable :: Integer
oddSquareSumReadable =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

numLongChain :: Int -> Int -> Int -> Int
numLongChain minLength start end = length . filter (\x -> (length x) > minLength) . map chain $ [start..end]
