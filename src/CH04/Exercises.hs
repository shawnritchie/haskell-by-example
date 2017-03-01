module CH04.Exercises where

import Data.Char (digitToInt, isDigit)
import Control.Exception
import CH04.SplitLines

type ErrorMessage = String

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

asInt :: String -> Int
asInt xs = loop 0 xs

asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold (x:xs)
  | x == '-' = negate $ foldl aggregator 0 xs
  | otherwise = foldl aggregator 0 (x:xs)
  where
      aggregator acc x = acc * 10 + digitToInt x

safeAsInt_fold :: String -> Either ErrorMessage Int
safeAsInt_fold [] = Right 0
safeAsInt_fold str = let allDigits (x:xs) = all isDigit xs && (x == '-' || isDigit x)
  in case allDigits str of
    True -> Right $ asInt_fold str
    False -> Left $ "non-digit '" ++ [(head $ filter (not . isDigit) str)] ++ "'"

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile fx = reverse . fst . foldl conditionalFx ([], True)
  where conditionalFx (agg, continue) v
          | fx v == True = ((v:agg), continue)
          | otherwise = (agg, False)

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy fx = foldr aggregator []
  where
    aggregator val [] = [[val]]
    aggregator val agg@(x:xs)
      | (fx val $ head x) == True = ((val:x):xs)
      | otherwise = [val] : agg


myAny :: Foldable t => (a -> Bool) -> t a -> Bool
myAny fx = foldr anyer False
  where anyer val agg
          | agg == True = True
          | otherwise = fx val

myCycle :: [a] -> [a]
myCycle rs = foldr repeater rs [1..]
  where repeater _ agg = agg ++ agg

myWords :: String -> [String]
myWords = foldr folder []
  where folder c [] = [[c]]
        folder c agg = [c:(head agg)]


myUnlines :: [String] -> String
myUnlines = foldr (\val agg -> val ++ ('\n':agg)) ""