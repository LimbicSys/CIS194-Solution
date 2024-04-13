import Data.List
import qualified Data.Set as Set

-- exercise 1.1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' lst = foldr (\x acc -> (x - 2) * acc) 1 $ filter even lst

-- exercise 1.2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n =
    sum $
        filter even $
            takeWhile (\x -> x /= 1) $
                iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n

-- exercise 2
data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs =
    Node
        height
        (foldTree $ take half xs)
        (xs !! half)
        (foldTree $ drop (half + 1) xs)
  where
    len = length xs
    half = len `div` 2
    height = floor $ logBase 2 (fromIntegral len)

-- exercise 3.1
xor :: [Bool] -> Bool
xor xs =
    foldr
        ( \x1 x2 ->
            case (x1, x2) of
                (True, False) -> True
                (False, True) -> True
                otherwise -> False
        )
        False
        xs

-- exercise 3.2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

-- exercise 3.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base $ reverse xs

-- exercise 4
-- Start with a list of the integers from 1 to n.
-- From this list, remove all numbers of the form i + j + 2ij where:
-- i , j ∈ N ,   1 ≤ i ≤ j {\displaystyle i,j\in \mathbb {N} ,\ 1\leq i\leq j}
-- i + j + 2 i j ≤ n {\displaystyle i+j+2ij\leq n}
-- The remaining numbers are doubled and incremented by one,
-- giving a list of the odd prime numbers (i.e., all primes except 2) below 2n + 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    [2 * x + 1 | x <- [1 .. n], Set.notMember x numdel]
  where
    numdel =
        Set.fromList
            [ i + j + 2 * i * j
            | let n' = fromIntegral n
            , i <- [1 .. floor (sqrt (n' / 2))]
            , let i' = fromIntegral i
            , j <- [i .. floor ((n' - i') / (2 * i' + 1))]
            ]
