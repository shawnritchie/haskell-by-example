module CH04.SplitLines where

import Data.Maybe
import Data.List

splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String
fixLines input = unlines (splitLines input)

unsafeListHandling :: [a] -> ([a] -> b) -> Maybe b
unsafeListHandling xs fx = if null xs then Nothing else Just $ fx xs

safeHead :: [a] -> Maybe a
safeHead xs = unsafeListHandling xs head

safeTail :: [a] -> Maybe [a]
safeTail xs = unsafeListHandling xs tail

safeLast :: [a] -> Maybe a
safeLast xs = unsafeListHandling xs last

safeInit :: [a] -> Maybe [a]
safeInit xs = unsafeListHandling xs init

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith fx lines =
  let (pre, suf) = break fx lines
  in  pre : case suf of
              (x:rest) -> splitWith fx rest
              _ -> []

transposes :: String -> String
transposes [] = []
transposes str =
      let
          lines = filter (/=[]) $ splitLines str
          nextIteration = unlines $ map tail lines
      in
          '\n' : map head lines ++ transposes nextIteration