{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative

import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
        | p x = Just (x, xs)
        | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
        | null ns = Nothing
        | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- exercise 1
first :: (a -> b) -> (a, c) -> (b, c)
first f (l, r) = (f l, r)

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f parserA = Parser parserFuncB
      where
        parserFuncA = runParser parserA
        parserFuncB str = fmap (first f) (parserFuncA str)

-- exercise 2
instance Applicative Parser where
    -- NOTE: pure f <*> x = fmap f x
    pure f = Parser (\str -> Just (f, str))

    pa <*> pb = Parser parserFuncC
      where
        parserFuncA = runParser pa
        parserFuncC str = case parserFuncA str of
            Nothing -> Nothing
            Just (f, str') -> runParser (f <$> pb) str'

-- exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

-- exercise 4
instance Alternative Parser where
    empty = Parser (\str -> Nothing)

    (Parser pa) <|> (Parser pb) = Parser $ (<|>) <$> pa <*> pb

-- exercise 5
posInt' = (\_ -> ()) <$> posInt
posUpper = (\_ -> ()) <$> satisfy isUpper
intOrUppercase :: Parser ()
intOrUppercase = posInt' <|> posUpper
