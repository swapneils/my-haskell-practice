module REP where

import Control.Applicative
import Parser
import RE


pAlphabet :: Parser Char Alphabet
pAlphabet = A <$ sym 'A'
    <|> B <$ sym 'B'
    <|> C <$ sym 'C'


pEscChar :: Parser Char Char
pEscChar = Parser chomp
    where
    chomp ('\\':c:cs) = [(c,cs)]
    chomp (c:cs) | c `notElem` "\\|+*e0()" = [(c,cs)]
    chomp _ = []

{-

RE  ::= RE2 "|" RE 
      | RE2
RE2 ::= RE3 RE2
      | RE3
RE3 ::= RE3 "*"
      | RE3 "+"
      | RE4
RE4 ::= Symbol 
      | "e"
      | "0"
      | "(" RE ")"

-}

pREof :: Parser Char sym -> Parser Char (RE sym)
pREof pSym = pRE
    where
    
    pRE = RAlt <$> pRE2 <* sym '|' <*> pRE
        <|> pRE2
    
    pRE2 = RSeq <$> pRE3 <*> pRE2
        <|> pRE3
    
    pRE3 = foldl (\r f -> f r) <$> pRE4 <*> many pStars
    
    pStars = RStar <$ sym '*'
        <|> RPlus <$ sym '+'
    
    pRE4 = RSym <$> pSym
        <|> REps <$ sym 'e'
        <|> RZero <$ sym '0'
        <|> sym '(' *> pRE <* sym ')'


pREof' :: Parser Char sym -> Parser Char (RE sym)
pREof' pSym = pRE
    where
    
    pRE = pRE2 <**> (sym '|' *> (flip RAlt <$> pRE) <|> pure id)
    
    pRE2 = pRE3 <**> (flip RSeq <$> pRE2 <|> pure id)
    
    pRE3 = pRE4 <**> pStars
    
    pStars = sym '*' *> ((RStar .) <$> pStars)
        <|> sym '+' *> ((RPlus .) <$> pStars)
        <|> pure id
    
    pRE4 = RSym <$> pSym
        <|> REps <$ sym 'e'
        <|> RZero <$ sym '0'
        <|> sym '(' *> pRE <* sym ')'
