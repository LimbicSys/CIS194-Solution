{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Read
import Data.List (intercalate, takeWhile)

-- exercise 1
parseErrorLevel :: [String] -> Maybe Int
parseErrorLevel [] = Nothing
parseErrorLevel (x:_) = readMaybe x :: Maybe Int

parseMessageType :: [String] -> Maybe MessageType
parseMessageType ("I":_) = Just Info
parseMessageType ("W":_) = Just Warning
parseMessageType ("E":xs) = case parseErrorLevel xs of
    Just level -> Just $ Error level
    Nothing -> Nothing
parseMessageType _ = Nothing

parseTimeStamp :: [String] -> Maybe TimeStamp
parseTimeStamp [] = Nothing
parseTimeStamp (x:_) = readMaybe x


workListToString :: [String] -> String
workListToString = intercalate " "

parseMessageFromList :: [String] -> LogMessage
parseMessageFromList lst = case (parseMessageType lst) of
    Nothing -> Unknown $ workListToString lst
    Just msgTy ->
        case parseTimeStamp restList of
            Just time -> case restList of
                [] -> Unknown $ workListToString lst
                _ -> LogMessage msgTy time $ workListToString $ tail restList
            Nothing -> Unknown $ workListToString lst
        where restList = case msgTy of
                Error _ -> tail $ tail lst
                _ -> tail lst

parseMessage :: String -> LogMessage
parseMessage = parseMessageFromList . words

parse :: String -> [LogMessage]
parse logs = foldl (\lst line -> parseMessage line : lst) [] $ lines logs

-- exercise 2
timeStamp :: LogMessage -> Maybe TimeStamp
timeStamp (Unknown _) = Nothing
timeStamp (LogMessage _ time _) = Just time

leftNode :: MessageTree -> MessageTree
leftNode Leaf = Leaf
leftNode (Node l m r) = l

rightNode :: MessageTree -> MessageTree
rightNode Leaf = Leaf
rightNode (Node l m r) = r

insert :: LogMessage -> MessageTree -> MessageTree
insert msg tr = case timeStamp msg of
    Nothing -> tr
    Just time -> case tr of
        Leaf -> Node Leaf msg Leaf
        (Node leftTr m righTr) ->
            if Just time <= nodeTime then
                Node (insert msg $ leftNode tr) m righTr
            else
                Node leftTr m (insert msg $ rightNode tr)
            where nodeTime = timeStamp m

-- exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ (m : inOrder r)

-- exercise 5
msgFilter :: LogMessage -> Bool
msgFilter (Unknown _) = False
msgFilter (LogMessage ty _ _) = case ty of
    (Error level) -> level >= 50
    _ -> False

whatWentWrongMsg :: [LogMessage] -> [LogMessage]
whatWentWrongMsg lst = filter msgFilter $ inOrder $ build lst

msgContent :: LogMessage -> String
msgContent (Unknown s) = s
msgContent (LogMessage ty time s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lst = map msgContent $ whatWentWrongMsg lst
