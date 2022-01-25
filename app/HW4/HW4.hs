-- Submit this file.

module HW4 where

import RE
    -- download RE.hs from Canvas and put it in the same directory as this file

-- Part I: Regular Expressions
-- ---------------------------

-- In this section, you will write four regular expressions using a variant on the
-- notation used in class: specifically, use + to indicate one or more repetitions,
-- e to indicate an empty string, and 0 to indicate the empty language.
--
-- For example, (AB+)|e is a regular expression that matches the empty string, or strings
-- containing an A followed by one or more B's.
--
-- This is the same notation used by the regular expression parser provided in REP.hs.
-- I recommend using the parser and matcher to test your regular expressions.
--
-- *REP> parse (pREof pAlphabet) "(AB+)|e"
-- Just (RAlt (RSeq (RSym A) (RPlus (RSym B))) REps)
-- *REP> Just r = parse (pREof pAlphabet) "(AB+)|e"
-- *REP> match r [A,B,B]
-- True
--
-- See HW4_test.hs for more tools you may find helpful for testing.

-- 1. Create a regex for the language of all strings that begin with A and end with B and
-- do not contain C.

regex1 :: [Char]
regex1 = "A(A|B)*B"

-- 2. Create a regex for the language of all strings that contain an even number of A's,
-- along with any number of B's and C's.

regex2 :: [Char]
regex2 = "(A(B|C)*A|B|C)*|e"

-- 3. Create a regex for the language of all strings that begin and end with two B's.

regex3 :: [Char]
regex3 = "BB(e|(A|B|C)*BB)"

-- 4. Create a regex for the language of strings containing at least one A and one B,
-- where all A's are earlier in the string than all B's.

regex4 :: [Char]
regex4 = "C*A(A|C)*B(B|C)*"



-- Part II: Manipulating Regular Expressions
-- -----------------------------------------

-- 5. matchEmpty tests whether the language for a regular expression includes the empty
-- string.
-- 
-- Note that the type signature does not include Eq sym. This means that you will not be
-- able to use match or matchPrefix.

matchEmpty :: RE sym -> Bool
matchEmpty (RSeq a b) = matchEmpty a && matchEmpty b
matchEmpty (RAlt a b) = matchEmpty a || matchEmpty b
matchEmpty (RStar _) = True
matchEmpty REps = True
matchEmpty _ = False


-- 6. firsts re returns a list containing every symbol that occurs first in some string
-- in the language for re.
--
-- For example, if re represents "A(C|B)|BC", then the strings in its language are AB, AC,
-- and BC. In this case, firsts re might return [A,B].
--
-- Note that the type signature does not include Eq sym or Ord sym. This means that your
-- code will be unable to sort or remove duplicates from the list of symbols it returns.
-- The requirements your code must satisfy are:
-- 1. the list returned must be finite (even if the language is infinite!)
-- 2. every symbol in the list must be the first symbol in some string in the language
-- 3. for every string in the language, its first symbol must occur in the list
-- Individual symbols may occur in any order, and may be duplicated any finite number of
-- times.

firsts :: RE sym -> [sym]
firsts (RSym a) = [a]
firsts REps = []
firsts RZero = []
firsts (RStar a) = firsts a
firsts (RPlus a) = firsts a
firsts (RAlt a b) = (firsts a) ++ (firsts b)
firsts (RSeq a b) = if matchEmpty a then (firsts a) ++ (firsts b) else firsts a
  -- where
  --   retest (RSym _) = False
  --   retest (RStar _) = True
  --   retest REps = True
  --   retest (RAlt x y) = retest x || retest y
  --   retest (RSeq x y) = retest x && retest y
  --   retest RZero = False
  --   retest (RPlus _) = False
        
-- Note that firsts will return an empty list if and only if the language for the regular
-- expression contains no strings.
