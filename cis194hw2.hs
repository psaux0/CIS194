{-
Name: Yuchen Su
Collaborators: None
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:xs) ys
  | elem x ys == True = formableBy xs (delete x ys)
  | otherwise = False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] hand [] = True
wordFitsTemplate (x:_) hand [] = False
wordFitsTemplate [] hand (y:_) = False
wordFitsTemplate (x:xs) hand (y:ys)
  | x == '?' = case elem y hand of True -> wordFitsTemplate xs (delete y hand) ys
                                   False -> False
  | otherwise = (x == y) && wordFitsTemplate xs hand ys

wordFittingTemplate :: Template -> Hand -> [String]
wordFittingTemplate temp hand = filter (wordFitsTemplate temp hand) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord xs = foldr (\x acc -> scrabbleValue x + acc) 0 xs

--have to use head, because acc is [String], but scrabbleValueWorld only takes Strin
bestWords :: [String] -> [String]
bestWords xs = foldr (\x acc -> accum x acc) [""] xs -- to use head, it should be [""]
                 where accum x acc 
                        | (scrabbleValueWord x) == (scrabbleValueWord $ head acc) = x : acc
                        | (scrabbleValueWord x) < (scrabbleValueWord $ head acc) = acc
                        | (scrabbleValueWord x) > (scrabbleValueWord $ head acc) = [x]
