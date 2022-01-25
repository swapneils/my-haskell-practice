module RE where

splits :: [a] -> [ ([a], [a]) ]
splits []     = [([],[])]
splits (x:xs) = ([],x:xs) : map (\(u,v) -> (x:u, v)) (splits xs)


data RE sym
    = RSym sym
    | REps
    | RZero
    | RStar (RE sym)
    | RPlus (RE sym)
    | RAlt (RE sym) (RE sym)
    | RSeq (RE sym) (RE sym)
    deriving (Show)

match :: (Eq sym) => RE sym -> [sym] -> Bool
match (RSym c) [s] = s == c
match REps     []  = True
match (RAlt a b) s = match a s || match b s
match (RSeq a b) s = any (\(u,v) -> match a u && match b v) (splits s)
match (RStar a) [] = True
match (RStar a) (s:ss) 
    = any (\(u,v) -> match a (s:u) && match (RStar a) v) (splits ss)
match (RPlus a) s = match (RSeq a (RStar a)) s
match _          _ = False

matchPrefix :: (Eq sym) => RE sym -> [sym] -> Bool
matchPrefix re = any (null . snd) . suffixes re

suffixes :: (Eq sym) => RE sym -> [sym] -> [(Bool, [sym])]
suffixes (RSym c) (s:ss) | c == s = [(True,ss)]
suffixes REps     s = [(False,s)]
suffixes (RAlt a b) s = suffixes a s ++ suffixes b s
suffixes (RSeq a b) s = [ (p || q, v) | (p,u) <- suffixes a s, (q,v) <- suffixes b u]
suffixes (RStar a) s = (False, s) : [ (True,v)
    | (True, u) <- suffixes a s   -- only cases where a matched a non-empty prefix
    , (_, v)    <- suffixes (RStar a) u
    ]
suffixes (RPlus a) s = suffixes (RSeq a (RStar a)) s
suffixes _ _ = []


data Alphabet = A | B | C deriving (Show, Eq)

ab = RSeq (RSym A) (RSym B)

abstar = RStar ab

aopt = RAlt (RSym A) REps

aoptstar = RStar aopt
