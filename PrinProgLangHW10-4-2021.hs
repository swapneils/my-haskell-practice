heads :: [a] -> Maybe a
heads (x:_) = Just x
heads _ = Nothing

final :: [a] -> Maybe a
final (x:xs) = if null xs then Just x else final xs
final _ = Nothing


data Tree a = Tip | Bin (Tree a) a (Tree a)
sumTree :: Num a => Tree a -> a
sumTree Tip = 0
sumTree (Bin x y z) = y + sumTree x + sumTree z
