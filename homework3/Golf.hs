import Data.List
import qualified Data.Map as Map

-- exercise 1
filterWithIndex :: [a] -> Int -> [a]
filterWithIndex lst m = snd $ unzip $ filter (\(i, _) -> i `mod` m == 0) $ zip [1..] lst

skips :: [a] -> [[a]]
skips lst = map (filterWithIndex lst) [1..length lst]


-- exercise 2
sublists :: Int -> [a] -> [[a]]
sublists n xs = take (length xs - n + 1) $ sublists' n xs
    where sublists' _ [] = [[]]
          sublists' n xs@(_:rest) = take n xs : sublists' n rest

localMaxima :: [Integer] -> [Integer]
localMaxima lst = map (\(_:y:_) -> y)
    $ filter (\(x:y:z:[]) -> y > x && y > z)
    $ filter (\x -> length x == 3)
    $ sublists 3 lst


-- exercise 3
counterFromList :: [Integer] -> Map.Map Integer Int
counterFromList = foldr (\k acc -> Map.insertWith (+) k 1 acc) Map.empty

repeatedStr :: Int -> String -> String
repeatedStr n v = concat $ replicate n v

keyToString :: Map.Map Integer Int -> Integer -> String
keyToString d k = case Map.lookup k d of
    Nothing -> s
    (Just count) -> s ++ (repeatedStr count "*")
    where s = show k ++ "="

maxLength :: Map.Map Integer Int -> Int
maxLength dict = case (filter (\v -> v >= 0 && v <= 9) $ Map.elems dict) of
    [] -> baseLen
    all@(x:xs)-> baseLen + maximum all
    where baseLen = 2

counterToString :: Map.Map Integer Int -> [String]
counterToString dict =
    let noPadding = map (keyToString dict) [0..9] in
        let maxLen = maxLength dict in
            map (\s -> let len = (length s) in
                if len < maxLen then
                    s ++ (repeatedStr (maxLen - len) " ")
                else
                    s)
                noPadding

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

histogram :: [Integer] -> String
histogram lst = concat $ map (\s -> s ++ "\n") $ rotate $ counterToString $ counterFromList lst

