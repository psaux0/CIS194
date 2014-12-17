module CIS194HW4 where

import BST
import qualified Data.Char as DChar

-- Binary Tree
-- data BST a = Leaf | Node (BST a) a (BST a)
-- Insert an element
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST f x (Node left y right)
  | f x y == EQ = Node left y right
  | f x y == GT = Node left y (insertBST f x right)
  | f x y == LT = Node (insertBST f x left) y right
  | otherwise = error "Error!"

-- Visiting the Library
allCaps :: [String] -> Bool
allCaps [] = True
allCaps (x:xs)
  | x == "" = True && allCaps xs
  | otherwise = (all DChar.isUpper x) && allCaps xs

-- Drop the trailing whitespace from a string
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace xs = reverse $ (dropWhitespace . reverse) xs
  where dropWhitespace y@(x:xs)
          | x == ' ' = dropWhitespace xs
          | otherwise = y
        dropWhitespace [] = []

-- Getting the first letters of a list of a string
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

validHead :: Maybe a -> a
validHead (Just a) = a
validHead Nothing = error "Nothing"

firstLetters :: [String] -> [Char]
firstLetters xs = foldr headAccum [] xs
  where headAccum x acc
            | (safeHead x == Nothing) = acc
            | otherwise = (validHead $ safeHead x) : acc

asList :: [String] -> String
asList xs = foldr (\x acc -> if x == '\"' then acc else x : acc) [] ys
              where ys = show xs
