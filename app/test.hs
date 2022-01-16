{-# LANGUAGE Safe #-}

module Examples where
-- import System.Random
-- import safe Data.Deriving
import safe Data.Eq
import safe Data.Ord
import safe Data.Function
import safe Data.List
import safe Data.Maybe
import safe Data.Char
import safe Control.Monad

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

for list action = mapM_ action list

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
-- mean of [Fractional]
foo :: (Fractional a) => [a] -> a
foo = uncurry (/) . foldr (\x (s,l) -> (s + x, l + 1)) (0, 0)

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


data BST a = BinaryTree (BST a) a (BST a) | Tip
bstinorder Tip = []
bstinorder (BinaryTree a x b) = bstinorder a ++ [x] ++ bstinorder b


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

data RunLengthEncoder a = Multiple Int a | Single a
  deriving (Show)
modrunlength xs = map modrunlengthhelper $ runlength xs
  where
    modrunlengthhelper (1,x) = Single x
    modrunlengthhelper (n,x) = Multiple n x
decodemodrunlength xs = foldr decodemodrunlengthhelper [] xs
  where
    decodemodrunlengthhelper (Single x) acc = x:acc
    decodemodrunlengthhelper (Multiple n x) acc = concat [(take n $ repeat x),acc]

-- modrunlength "direct solution"

dupli [] = []
dupli [x] = [x,x]
dupli (x:xs) = [x,x] ++ dupli xs

repli [] _ = []
repli [x] n = (take n (repeat x))
repli (x:xs) n = ((take n . repeat) x) ++ repli xs n

dropRepeat inp x = dropRepeatHelper inp x 1
  where dropRepeatHelper [] _ _ = []
        dropRepeatHelper (x:xs) n i =
          (if (rem i n) == 0 then [] else [x]) ++ dropRepeatHelper xs n (i+1)

split (x:xs) n = split' (x:xs) n []
  where split' xs 0 acc = [reverse acc,xs]
        split' [] _ acc = [reverse acc, []]
        split' (x:xs) n acc = split' xs (n-1) (x:acc)

slice [] _ _ = []
slice _ _ 0 = []
slice (x:xs) 1 n = x:slice xs 1 (n-1)
slice (_:xs) m n = slice xs (m-1) (n-1)

rotate [] _ = []
rotate inp n = rotate' inp (mod n (length inp)) []
  where rotate' xs 0 acc = xs ++ reverse acc
        rotate' [] n acc = rotate' (reverse acc) n []
        rotate' (x:xs) n acc = rotate' xs (n-1) (x:acc)

-- Incomplete and inelegant:
-- removeAt inp n = (take (i-1) inp) ++ drop i inp
--   where i = if n<0 then length inp + (n+1) else n
-- This version is better (taken from the solutions):
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = case back of
        [] -> error "removeAt: index too large"
        x:rest -> (x, front ++ rest)
  where (front, back) = splitAt (k - 1) xs

insertAt x k xs = front ++ [x] ++ back
  where (front, back) = splitAt (k - 1) xs

range a b = [a..b]


-- main = do
--     putStrLn "What is your name?"
--     name <- getLine
--     let caps = map toUpper name
--     putStr "Hello, "
--     putStrLn caps
--     putStrLn $ show $ packlist caps
--     putStrLn $ show $ modrunlength caps
--     putStrLn (decodemodrunlength $ modrunlength caps)

member x []     = False
member x (z:xs) = z == x || member x xs

hasRepeats :: Eq a => [a] -> Bool
-- hasRepeats inp = or $ map (\x -> let temp = removeAt x inp in member (fst temp) (snd temp)) [1..(length inp)]
hasRepeats = fst . (foldr (\a (b, l) -> if b then (True, []) else ((member a l) , (a:l))) (False, []))

listIsEmpty [] = True
listIsEmpty (x:xs) = False

main = do
  inp <- getLine
  let times = read inp in do
                        for [1..times] (\i -> do
                                           input <- getLine
                                           putStrLn $ show (replacePairs (map digitToInt input))
                             )
  return ""

-- replacePairs :: [Int] -> Int
replacePairs inp = if (1 >= length inp)
                   then (shower inp)
                   else foldl (\acc k -> max acc (shower (inserter inp k))) (shower (inserter inp 1)) [2..((length inp) - 1)]

  where shower xs = read (foldr (++) "" $ map show xs) :: Integer
        inserter a k = (let (front, back) = splitAt (k-1) a in front ++ [head back + head (drop 1 back)] ++ (drop 2 back))
   -- replacePairs' st = foldl (++) "" $ replacePairs'' st []
        -- replacePairs'' [] acc = reverse (map show acc)
        -- replacePairs'' (a:b:xs) [] = replacePairs'' xs [a + b]
        -- replacePairs'' (x:xs) acc = replacePairs'' xs (x+(head acc) : acc)
    -- replacePairs''' (x:y:xs) (a:b:acc) = (show (a+b) : replacePairs''' (y:xs) (b:acc))
    -- replacePairs''' (x:xs) acc = []
    -- replacePairs'''' (a:b:xs) = max (read (a:(show (replacePairs'''' (b:xs)))) :: Int)
    --                                 (read (show (digitToInt a + digitToInt b) ++ xs) :: Int)
    -- replacePairs'''' (x:xs) = 0
    -- replacePairs'''' [] = 0
