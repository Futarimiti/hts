module Main (main) where

import Control.Applicative      (Alternative (..))
import Control.Monad            (guard)
import Data.Char                (digitToInt, isDigit)
import Data.Monoid              (Sum (..))
import System.Environment.Blank (getArgs)
import System.Hclip             (setClipboard)

main :: IO ()
main = do
  [str] <- getArgs
  let
    cxp = getCxP str
    first25 = take 25 $ filter (not . isDigit) str
    answer = map succ first25 ++ show cxp
  setClipboard answer
  putStrLn answer
  putStrLn "(answer also copied to clipboard)"

-- for every digit in the string, find composite and prime numbers
-- and sum them both and multiply
getCxP :: String -> Int
getCxP str = getSum . uncurry (*) . mconcat $ do
  char <- str
  n <- if isDigit char
    then pure $ digitToInt char
    else empty
  guard $ n > 1
  pure $ if isPrime n
    then (Sum n, 0)
    else (0, Sum n)

isPrime :: Integral n => n -> Bool
isPrime n
  | n < 2     = False
  | otherwise = null [ x | x <- [2 .. isqrt n], n `mod` x == 0 ]
  where
    isqrt :: (Integral a, Integral b) => a -> b
    isqrt = floor . sqrt . fromIntegral


