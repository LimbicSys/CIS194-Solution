{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import Data.Map as M
import ExprT
import Parser
import qualified StackVM as VM

-- exercise 1
eval :: ExprT -> Integer
eval (Lit l) = l
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parsedExpr of
    Just e -> Just $ eval e
    Nothing -> Nothing
  where
    parsedExpr = parseExp Lit Add Mul s

-- exercise 3
class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a

instance Expr ExprT where
    mul x1 x2 = Mul x1 x2
    add x1 x2 = Add x1 x2
    lit num = Lit num

reify :: ExprT -> ExprT
reify = id

-- exercise 4
instance Expr Integer where
    mul x1 x2 = x1 * x2
    add x1 x2 = x1 + x2
    lit num = num

instance Expr Bool where
    mul x1 x2 = x1 && x2
    add x1 x2 = x1 || x2
    lit num = if num > 0 then True else False

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    mul (MinMax x1) (MinMax x2) = MinMax $ min x1 x2
    add (MinMax x1) (MinMax x2) = MinMax $ max x1 x2
    lit num = MinMax num

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    mul (Mod7 x1) (Mod7 x2) = Mod7 $ (x1 * x2) `mod` 7
    add (Mod7 x1) (Mod7 x2) = Mod7 $ (x1 + x2) `mod` 7
    lit num = Mod7 $ num `mod` 7

-- test
testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- exercise 5
instance Expr VM.Program where
    mul e1 e2 = e1 ++ e2 ++ [VM.Mul]
    add e1 e2 = e1 ++ e2 ++ [VM.Add]
    lit num = [VM.PushI num]

compile :: String -> Maybe VM.Program
compile s = parseExp lit add mul s

-- exercise 6
class Hasvars a where
    var :: String -> a

instance Hasvars (M.Map String Integer -> Maybe Integer) where
    var s = \m -> M.lookup s m

instance Expr (M.Map String Integer -> Maybe Integer) where
    mul e1 e2 = \m -> do
        v1 <- e1 m
        v2 <- e2 m
        Just (v1 * v2)
    add e1 e2 = \m -> do
        v1 <- e1 m
        v2 <- e2 m
        Just (v1 + v2)
    lit num = \_ -> Just num

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
