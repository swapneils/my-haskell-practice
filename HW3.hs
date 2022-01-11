module HW3 where
import Data.List

-- Introduction
-- ------------

-- Recall the function foldr
--
--     foldr :: (a -> b -> b) -> b -> [a] -> b
--     foldr f z []     = z
--     foldr f z (a:as) = f a (foldr f z as)
--
-- All functions that consume a list can be written using foldr with appropriate arguments
--
--     sum :: (Num a) => [a] -> a
--     sum = foldr (+) 0

-- For clarity, we could use where to name the function and default values passed to foldr
-- 
--     sum = foldr f z
--         where
--         f a b = a + b
--         z = 0
--
-- We will use this pattern to define several functions as folds over other data
-- structures.


-- Part I
-- ------

data Tree a 
    = Tip
    | Bin (Tree a) a (Tree a)
    deriving (Show, Eq)


foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree _ z Tip         = z
foldTree f z (Bin l x r) = f (foldTree f z l) x (foldTree f z r)


-- For each of the following functions, define f and z such that the overall function
-- computes the desired result
--
-- You are permitted to add parameters to f.
--
-- Hint: for recent versions of GHC, you can replace "undefined" with "_". This will
-- cause GHC to print out the required type and the types of any local variables in
-- scope. You may find this helpful to guide your development.

-- sumTree t returns the sum of all values stored in the tree
sumTree :: (Num a) => Tree a -> a
sumTree = foldTree f z
    where
    -- f :: a -> a -> a -> a
    f b c d = b + c + d
    -- hint: you will most likely rewrite this in the form "f x y z = something"

    -- z :: a
    z = 0


-- minTree t returns Nothing if t is empty, or Just x if x is the smallest value in t
minTree :: (Ord a) => Tree a -> Maybe a
minTree = foldTree f z 
  where
    z = Nothing
    f (Just b) c (Just d) = if cach < d then Just cach else Just d
      where
        cach = if b < c then b else c
    f Nothing c (Just d) = if c < d then Just c else Just d
    f (Just b) c Nothing = if b < c then Just b else Just c
    f Nothing c Nothing = Just c

-- listTree t returns a list containing all the elements of t, in order
listTree :: Tree a -> [a]
listTree = foldTree f z
  where
    f b c d = b ++ [c] ++ d
    z = []
    

-- Part II
-- -------

data Rose a = Node a [Rose a]
    deriving (Show, Eq)

-- foldRose is the fold over a rose tree
foldRose :: (a -> [b] -> b) -> Rose a -> b
foldRose f (Node x []) = f x []
foldRose f (Node x rs) = f x (map (foldRose f) rs)

-- sumRose t returns the sum of all values stored in the tree
sumRose :: (Num a) => Rose a -> a
sumRose = foldRose f
    where
      f b c = b + foldr (+) 0 c
    
-- listRose t returns all values in pre-order (i.e. the value at the node occurs before
-- any child values)
listRose :: Rose a -> [a]
listRose = foldRose f
    where
    f b c = b : (foldr (++) [] c)

-- Sample data
-- -----------

tree1 :: Tree Int
tree1 =
    Bin
        (Bin Tip 4 Tip)
        3
        (Bin
            (Bin Tip 2 Tip)
            7
            Tip)

-- we should have
--   sumTree tree1 == 16
--   minTree tree1 == Just 2
--   listTree tree1 == [4,3,2,7]

rose1 :: Rose Int
rose1 = Node 18
    [ Node 7 []
    , Node 4 [Node 2 [], Node 100 []]
    , Node 8 []
    , Node 9 [Node 9 []]
    ]

-- we should have:
--   sumRose rose1 == 157
--   listRose rose1 ==[18,7,4,2,100,8,9,9]

-- Extra Credit I
-- --------------

-- Determine an appropriate type for minRose and write it using foldRose
minRose :: (Ord a) => Rose a -> a
minRose = foldRose f
  where
    f b [] = b
    f b c = min b (foldr1 min c)


-- Extra Credit II
-- ---------------

-- It is possible to define listTree so that it runs in linear time, but it requires
-- thinking about folds and their return values in a way that is not obvious. Can you
-- solve this riddle and find appropriate definitions?

-- listTree' :: Tree a -> [a]
-- listTree' t = foldTree f z t []
--     where
--     f b c d = [] b c d
--     (z) x y = x ++ y
