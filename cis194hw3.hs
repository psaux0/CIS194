{-# OPTIONS_GHC -Wall #-}
-- By Yuchen Su

module LogAnalysis where

import Log
import qualified Data.List as DL
import qualified Data.Char as DC

parseMessage :: String -> MaybeLogMessage
parseMessage xs
  | msgInfo == "E" = ValidLM (LogMessage (Error ((read . head) msgContent :: Int)) ((read . head . tail) msgContent :: Int) ((unwords . (drop 2)) msgContent))
  | msgInfo == "I" = ValidLM (LogMessage Info ((read . head) msgContent :: Int) ((unwords . tail) msgContent))
  | msgInfo == "W" = ValidLM (LogMessage Warning ((read . head) msgContent :: Int) ((unwords . tail) msgContent))
  | otherwise = InvalidLM xs
  where msgInfo = (head . words) xs
        msgContent = (tail . words) xs

validMessageOnly :: [MaybeLogMessage] -> [LogMessage]
validMessageOnly xs = foldr logify [] xs
  where logify (InvalidLM _) acc = acc
        logify (ValidLM x) acc = x : acc

parse :: String -> [LogMessage]
parse xs = validMessageOnly $ ((map parseMessage) . lines) xs

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ a _) (LogMessage _ b _) = a `compare` b

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages xs = DL.sortBy compareMsgs xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = foldr errorOver [] xs
  where errorOver (LogMessage (Error t) _ s) acc | t >= 50 = s : acc
                                                 | otherwise = acc
        errorOver _ acc = acc

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout xs ys = filter (isAbout xs) ys
  where isAbout ss (LogMessage _ _ ts) = DL.isInfixOf (map (DC.toUpper) ss) (map (DC.toUpper) ts)

(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x


-- Redefine whatWentWrong
whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced ss xs = foldr (accum ss) [] xs
  where accum ps x@(LogMessage _ _ strr) acc | (|||) (msgAbout ps) msgWrong x = strr : acc
                                             | otherwise = acc
                                             where msgAbout fs (LogMessage _ _ ts) = DL.isInfixOf (map (DC.toUpper) fs) (map (DC.toUpper) ts)
                                                   msgWrong (LogMessage (Error t) _ _) | t >= 50 = True
                                                                                       | otherwise = False
                                                   msgWrong _ = False
