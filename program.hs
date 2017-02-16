doubleMe x = x * 2
doubleUs x y = doubleMe x + doubleMe y

applyFunction :: (b -> c) -> (a -> b) -> a -> c
applyFunction f g x = f (g x)
