{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib i | i <- [0 ..]]

-- exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : fib2 0 1 where fib2 a b = (a + b) : fib2 b (a + b)

-- exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance (Show a) => Show (Stream a) where
    show s = show $ take 20 $ streamToList s

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 xs1) (Cons x2 xs2) = Cons x1 (Cons x2 $ interleaveStreams xs1 xs2)

ruler :: Stream Integer
ruler =
    interleaveStreams (streamRepeat 0) $
        streamMap f (streamFromSeed (+ 2) 2)
  where
    f x = if even x then 1 + (f $ x `div` 2) else 0

-- exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate n = streamMap (0 -) n
    (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
    (*) (Cons a as) bi@(Cons b bs) = Cons (a * b) ((fromInteger a) * bs + (as * bi))

instance Fractional (Stream Integer) where
    (/) (Cons a as) (Cons b bs) = q
      where
        c = a `div` b
        cs1 = fromInteger (1 `div` b)
        cs2 = as - q * bs
        q = Cons c $ cs1 * cs2

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

-- exercise 7
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
        Matrix
            (a11 * b11 + a21 * b12)
            (a11 * b12 + a21 * b22)
            (a21 * b11 + a22 * b12)
            (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = getFibFromMat $ (Matrix 1 1 1 0) ^ (n - 1)
  where
    getFibFromMat (Matrix x _ _ _) = x
