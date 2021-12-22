module Parser where
import Control.Applicative

newtype Parser sym a = Parser ([sym] -> [(a, [sym])])

    -- Produce a value of type a from a list of symbols
    -- provide "leftovers" that can be used by later parsers
    -- (this allows us to build a parser by combining sub-parsers

runParser :: Parser sym a -> [sym] -> [(a, [sym])]
runParser (Parser f) = f

-- return one way to parse the string, or Nothing if multiple parses exist
parse :: Parser sym a -> [sym] -> Maybe a
parse p s = case filter (\(_,l) -> null l) (runParser p s) of
    [(x,_)] -> Just x      -- if we parsed the whole string unambiguously, succeed
    _       -> Nothing    -- otherwise fail

-- return any ways to parse the complete string
parses :: Parser sym a -> [sym] -> [a]
parses p = map fst . filter (null . snd) . runParser p

sym :: (Eq sym) => sym -> Parser sym sym
sym x = Parser sym_
    where
    sym_ (y:ys) | x == y = [(x,ys)]    -- succeed if the first symbol is the one we want
    sym_ _ = []                        -- otherwise fail

instance Functor (Parser sym) where
    fmap f (Parser p) = Parser (map (\(a,s) -> (f a, s)) . p)
    -- apply a function to the result(s) of the parser

    -- e.g.,         sym 'x' :: Parser Char Char
    --       Var <$> sym 'x' :: Parser Char Expr

instance Applicative (Parser sym) where
    pure x = Parser (\s -> [(x,s)])   -- read no symbols, always succeed exactly once


    {-
    Parser p <*> Parser q = Parser (\s -> 
        concat 
        (map 
             (\(f,s') -> 
                map (\(x,s'') -> (f x, s'')) (q s')) 
             (p s)))
    -}

    Parser p <*> Parser q = Parser (\s -> 
        [ (f x, s'') | (f,s') <- p s, (x, s'') <- q s' ])

        -- comine sub-parsers: p <*> q first parse p, then parse q from the leftovers
        --      the result produced will be the function produced by p
        --      applied to the value produced by q
        --
        -- common pattern : f <$> p <*> q
        --      parse p followed by q, apply f to the values produced by p and q
        --
        -- related operators:
        --   p <* q  -- parse p followed by q, produce value produced by p
        --   p *> q  -- parse p followed by q, produce value produced by q


instance Alternative (Parser sym) where
    empty = Parser (\s -> [])
        -- parser that never succeeds

    Parser p <|> Parser q = Parser (\s -> p s ++ q s)
        -- choice between sub-parsers


{-   from Control.Applicative:

optional :: Parser sym a -> Parser sym (Maybe a)
optional p 
    =   Just <$> p 
    <|> pure Nothing
-}

{- what is optional p?

optional p
Just <$> p <|> pure Nothing
Parser (\s -> runParser (Just <$> p) s ++ runParser (pure Nothing) s)
Parser (\s -> runParser (Just <$> p) s ++ [(Nothing, s)])
Parser (\s -> map (\(x,s') -> (Just x,s')) (runParser p s) ++ [(Nothing, s)])


> runParser (optional (sym 'x')) "x"
[(Just 'x',""),(Nothing, "x")]
> runParser (optional (sym 'x')) "y"
[(Nothing, "y")]

-}

{-

example ::= var var
var ::= "x" | "y" | "z"

-}

example = (,) <$> pVar <*> pVar

pVar = sym 'x' <|> sym 'y' <|> sym 'z'



data Expr
    = Var Char
    | Add Expr Expr
    | Mult Expr Expr
    deriving (Show)

{-
expr ::= var | var "+" expr | var "*" expr
var ::= "x" | "y" | "z"
-}

pVar' = Var <$> pVar

pExpr = pVar'
    <|> (\e1 e2 e3 -> Add e1 e3)  <$> pVar' <*> sym '+' <*> pExpr
    <|> Mult <$> pVar' <* sym '*' <*> pExpr

-- f <$> p1 <*> p2 <*> p3 ...

{-

expr2 ::= term | term "+" expr2
term ::= factor | factor "*" term
factor ::= var | "(" expr ")"
var ::= "x" | "y" | "z"

-}

-- expr2 :: term |          term    "+"         expr2
pExpr2 = pTerm <|> Add <$> pTerm <* sym '+' <*> pExpr2
--                                ^ means we are discarding the '+' that sym will produce
--                 Add will be applied to the results from pTerm and pExpr2

-- term :: factor |           factor     "*"        term
pTerm = pFactor <|> Mult <$> pFactor <* sym '*' <*> pTerm

pFactor = pVar' <|> sym '(' *> pExpr2 <* sym ')'



