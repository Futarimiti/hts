module Main where

import Control.Monad.Reader
import Data.Function
import Data.List
import Data.Traversable
import Paths_l01            (getDataFileName)
import System.Environment
import System.Exit

-- Given a word list, find the first word that scrambles to the given word.
unscramble :: (Foldable t, Ord a, MonadReader (t [a]) m) => [a] -> m (Maybe [a])
unscramble = asks . find . scramblesTo

-- Determine whether the two given words are equal just not in order.
scramblesTo :: Ord a => [a] -> [a] -> Bool
scramblesTo = (==) `on` sort

main :: IO ()
main = do
  wordlist <- fmap lines . readFile =<< getDataFileName "wordlist.txt"
  inputs <- getArgs
  case for inputs $ \input -> unscramble input wordlist of
    Just outputs -> putStrLn $ intercalate "," outputs
    Nothing      -> die "bad input(s)"
