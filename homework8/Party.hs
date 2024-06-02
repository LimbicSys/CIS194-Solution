module Party where

import Data.List
import Data.Tree
import Employee

-- exercise 1.1
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL lst funScore) = GL (emp : lst) (funScore + empFun emp)

-- exercise 1.2
instance Semigroup GuestList where
    (<>) (GL lst1 funScore1) (GL lst2 funScore2) = GL (lst1 ++ lst2) (funScore1 + funScore2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2

-- exercise 2
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f e tr = f (rootLabel tr) $ [treeFold f e child | child <- subForest tr]

-- exercise 3
nextlevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextlevel emp pairLst =
    let (withBoss, withoutBoss) = unzip pairLst
     in let maxWithBoss = foldl' (\x (l, r) -> x <> max l r) mempty pairLst
         in (glCons emp $ mconcat withoutBoss, maxWithBoss)

-- exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tr = uncurry max res
  where
    res = treeFold nextlevel (mempty, mempty) tr

-- exercise 5

formatResult :: GuestList -> String
formatResult (GL lst funScore) = "Total fun: " ++ show funScore ++ "\n" ++ unlines emps
  where
    emps = sort [empName emp | emp <- lst]

computeResult :: String -> String
computeResult = formatResult . maxFun . read

main :: IO ()
main = readFile "company.txt" >>= (\tr -> putStrLn $ computeResult tr)
