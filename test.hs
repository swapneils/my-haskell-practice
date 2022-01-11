module Examples where
import Data.Ord
import Data.Function
import Data.List
import Data.Maybe
import Data.Char

square :: Integral a => a -> a
square x = x * x

face :: Int -> Bool
face x = 6 < x

sum_up_to n = loop 1 0
     where
     loop i sum
         | i <= n    = loop (i + 1) (sum + i)
         | otherwise = sum
-- flipper f a b = f b a

cartprod (a:b) (c:d) = concat [[(a,c)], (cartprod [a] d), (cartprod b (c:d))]
cartprod _ [] = []
cartprod [] _ = []

splits []     = [([],[])]
splits (c:cs) = ([],c:cs) : map (\(u,v) -> (c:u,v)) (splits cs)

neilreduce [] z _ = z
neilreduce [x] z f = f x z
neilreduce (a:b:ls) z f = (neilreduce ((f a b):ls) z f)

strcmp :: String -> String -> Bool
strcmp s1 s2 = case (s1, s2) of
        ([], []) -> True
        (s1:ss1, s2:ss2)
          | toUpper s1 == toUpper s2 ->
              strcmp ss1 ss2
          | otherwise -> False
        _ -> False

fizzbuzz n
  | mod n 15 == 0 = "fizzbuzz"
  | mod n 3  == 0 = "fizz"
  | mod n 5  == 0 = "buzz"
  | otherwise   = show n

qux = \f x -> f (f x)
foo :: (Fractional a) => [a] -> a
foo = uncurry (/) . foldl (\(s,l) x -> (s + x, l + 1)) (0, 0)

-- isNothing :: Maybe a -> Bool
-- isNothing Nothing = True
-- isNothing (Just _) = False

class Flavor a where
  flavor :: a -> String

instance Flavor Bool where
  flavor x = concat ["Sweet", bts x]
    where bts b = case b of
            True -> " YES!"
            False -> " NO!"

liftA2 f a b = f <$> a <*> b

-- Equivalent:
-- a = \x -> case x of [] -> True; (_:_) -> False
-- a = (\x -> let n=x in case n of [] -> True; (_:_) -> False)


-- From 99 Haskell Problems
lastinlist :: [a] -> Maybe a
lastinlist []     = Nothing
lastinlist [x]    = Just x
lastinlist (_:xs) = (lastinlist xs)

butlastinlist :: [a] -> Maybe a
butlastinlist [a,b] = Just a
butlastinlist (_:xs) = butlastinlist xs
butlastinlist _ = Nothing

elementat :: (Eq t, Num t) => [a] -> t -> Maybe a
elementat (x:_) 0 = Just x
elementat (_:xs) n = elementat xs (n - 1)
elementat [] _ = Nothing

lengthoflist xs = foldr (\x y -> 1 + y) 0 xs

reverselist xs = foldr (\x y -> y ++ [x]) [] xs

palindrome xs = xs == reverselist xs

data NestedList a = Elem a | List [NestedList a]
flattenlist :: NestedList a -> [a]
flattenlist (Elem x ) = [x]
flattenlist (List xs) =  foldr (++) [] $ map flattenlist xs

compresslist xs = foldr (\x y -> if (y == [] || x /= head y) then (x:y) else y) [] xs

packlist xs = foldr (\x acc -> if (acc == [] || x /= (head $ head acc)) then ([x]:acc) else ((x:(head acc)):(tail acc))) [] xs

runlength xs = foldr (\x acc -> ((length x, head x):acc)) [] $ packlist xs


main = do
    putStrLn "What is your name?"
    name <- getLine
    let caps = map toUpper name
    putStr "Hello, "
    putStrLn caps

member x []     = False
member x (z:xs) = z == x || member x xs
