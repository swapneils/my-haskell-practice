module Examples where

square :: Integral a => a -> a
square x = x * x

face :: Int -> Bool
face x = 6 < x

sum_up_to n = loop 1 0
     where
     loop i sum
         | i <= n    = loop (i + 1) (sum + i)
         | otherwise = sum
flip f a b = f b a


qux = \f x -> f (f x)
foo :: (Fractional a) => [a] -> a
foo = uncurry (/) . foldl (\(s,l) x -> (s + x, l + 1)) (0, 0)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

