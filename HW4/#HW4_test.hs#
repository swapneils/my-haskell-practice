-- Do not submit this file

module HW4_test where

import Parser
import RE
import REP
import HW4

-- Shorthand for parsing regular expressions over Alphabet

pRE :: Parser Char (RE Alphabet)
pRE = pREof pAlphabet

-- Parse regular expression and match against a string in one step
matchstr :: [Char] -> [Alphabet] -> Bool
matchstr re str = case parse pRE re of
    Just r -> matchPrefix r str
    _ -> False

-- This function indicates whether regex1 should accept a string
should_accept1 :: [Alphabet] -> Bool
should_accept1 s = length s > 1 && head s == A && last s == B && C `notElem` s

should_accept2 :: [Alphabet] -> Bool
should_accept2 s = length s == 0 || mod (countLetters s A) 2 == 0
countLetters :: [Alphabet] -> Alphabet -> Int
countLetters s c = length $ filter (== c) s

should_accept3 :: [Alphabet] -> Bool
should_accept3 s = (take 2 s) == [B, B] && (take 2 (reverse s)) == [B, B]

should_accept4 :: [Alphabet] -> Bool
should_accept4 s = (elem A s) && (elem B s) && (findIndex B s) > (findLastIndex A s)
findIndex :: Alphabet -> [Alphabet] -> Maybe Int
findIndex p xs =
  case [ i | (x, i) <- zip xs [0..], p == x ] of
    [] -> Nothing
    e:_ -> Just e
findLastIndex :: Alphabet -> [Alphabet] -> Maybe Int
findLastIndex p xs =
  case [ i | (x, i) <- zip (reverse xs) (reverse [0..(length xs - 1)]), p == x ] of
    [] -> Nothing
    e:_ -> Just e

generate :: RE sym -> [[sym]]
generate RZero      = []
generate REps       = [[]]
generate (RSym s)   = [[s]]
generate (RAlt a b) = interleave (generate a) (generate b)
generate (RSeq _ b) | null (generate b) = []
generate (RSeq a b) = intermix (map (\sa -> map (sa ++) (generate b)) (generate a))
generate (RStar a)  = [] : generate (RSeq a (RStar a))
generate (RPlus a)  = generate (RSeq a (RStar a))

interleave (a:as) bs = a : interleave bs as
interleave []     bs = bs

intermix = foldr interleave []

-- You may be able to write similar functions for regexes 2, 3, and 4.

-- You can use sequence and replicate to generate all strings of a
-- specified length.
--
-- *REP> sequence (replicate 2 [A,B,C])
-- [[A,A],[A,B],[A,C],[B,A],[B,B],[B,C],[C,A],[C,B],[C,C]]
--
-- If you have a function that says whether a string should be accepted, you can use it to
-- test your regex with large selections of strings.
--
-- *REP> Just r = parse (pREof pAlphabet) regex1
-- *REP> filter (\s -> matchPrefix r s /= should_accept1 s) (sequence (replicate 4 [A,B,C]))
-- []
