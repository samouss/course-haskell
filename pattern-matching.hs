head' :: [a] -> a
head' [] = error "Try to access first element of empty list!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of
  [] -> error "Try to access first element of empty list!"
  (x:_) -> x

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: (Num a) => [a] -> a
sum'' xs = case xs of
  [] -> 0
  (x:xs) -> x + sum'' xs

calculateBMI :: (RealFloat a) => a -> a -> String
calculateBMI weight height
  | bmi <= skinny = "Skinny boy!"
  | bmi <= normal = "Normal boy!"
  | bmi <= fat = "Fat boy!"
  | otherwise = "Stop eating mannn!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 18.5
        fat = 18.5

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ "." ++ [l]
