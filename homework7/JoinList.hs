{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JoinList where

import Buffer
import Data.List
import Data.Monoid
import Scrabble
import Sized

data JoinList m a
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- exercise 1
tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- exercise 2.1
getNodeSize :: (Sized b, Monoid b) => JoinList b a -> Size
getNodeSize jl = size $ tag jl

indexJ ::
    (Sized b, Monoid b) =>
    Int ->
    JoinList b a ->
    Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ x) = if i == 0 then Just x else Nothing
indexJ i (Append x jl1 jl2)
    | si >= Size 0 && si < size1 = indexJ i jl1
    | si < size x = indexJ (getSize (si - size1)) jl2
    | otherwise = Nothing
  where
    si = Size i
    size1 = getNodeSize jl1

-- exercise 2.2
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl
    | n <= 0 = jl
    | otherwise = case jl of
        Empty -> Empty
        sg@(Single _ _) -> if n == 1 then sg else Empty
        Append x jl1 jl2 ->
            if sn >= size x
                then Empty
                else
                    if sn > size1
                        then Empty +++ dropJ (getSize (sn - size1)) jl2
                        else dropJ n jl1 +++ jl2
          where
            sn = Size n
            size1 = getNodeSize jl1

-- exercise 2.3
takeJ ::
    (Sized b, Monoid b) =>
    Int ->
    JoinList b a ->
    JoinList b a
takeJ n jl
    | n <= 0 = Empty
    | otherwise = case jl of
        Empty -> Empty
        sg@(Single _ _) -> sg
        ap@(Append x jl1 jl2) ->
            if sn >= size x
                then ap
                else
                    if sn > size1
                        then jl1 +++ takeJ (getSize (sn - size1)) jl2
                        else takeJ n jl1
          where
            sn = Size n
            size1 = getNodeSize jl1

-- exercise 3
scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

-- exercise 4
instance Buffer (JoinList (Score, Size) String) where
    -- \| Convert a buffer to a String.
    toString Empty = []
    toString (Single _ s) = s
    toString (Append _ js1 js2) = toString js1 ++ toString js2

    -- \| Create a buffer from a String.
    fromString [] = Empty
    fromString xs = foldl' (+++) Empty (map (\str -> (Single (scoreString str, 1) str)) $ lines xs)

    -- \| Extract the nth line (0-indexed) from a buffer.  Return Nothing
    -- for out-of-bounds indices.
    line = indexJ

    -- \| @replaceLine n ln buf@ returns a modified version of @buf@,
    --   with the @n@th line replaced by @ln@.  If the index is
    --   out-of-bounds, the buffer should be returned unmodified.
    replaceLine n s jl =
        if (n < 0 || n >= numLines jl)
            then jl
            else
                let node = fromString s
                 in (takeJ n jl) +++ node +++ (dropJ (n + 1) jl)

    -- \| Compute the number of lines in the buffer.
    numLines Empty = 0
    numLines (Single _ s) = 1
    numLines (Append (_, sz) js1 js2) = getSize sz

    -- \| Compute the value of the buffer, i.e. the amount someone would
    --   be paid for publishing the contents of the buffer.
    value Empty = 0
    value (Single (sc, _) _) = getScoreValue sc
    value (Append (sc, _) _ _) = getScoreValue sc

-- for test
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

initialList :: JoinList (Score, Size) String
initialList =
    fromString "This buffer is for notes you don't want to save, and for"
        +++ fromString "evaluation of steam valve coefficients."
        +++ fromString "To load a different file, type the character L followed"
        +++ fromString "by the name of the file."
