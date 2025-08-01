module Main where

import Codec.Archive.Zip          qualified as Zip
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as BS8
import Data.Function
import Data.List
import Data.Traversable
import HTS.Utils                  qualified as HTS
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
  zip' <- HTS.fetch "https://www.hackthissite.org/missions/prog/1/wordlist.zip"
  Right archive <- pure $ Zip.toArchiveOrFail zip'
  Just entry <- pure $ Zip.findEntryByPath "wordlist.txt" archive
  let
    -- uses \r\n as line separator
    wordlistRaw = BS8.unpack $ Zip.fromEntry entry
    wordlist = map (dropWhileEnd (== '\r')) $ lines wordlistRaw
  inputs <- getArgs
  case for inputs $ \input -> unscramble input wordlist of
    Just outputs -> putStrLn $ intercalate "," outputs
    Nothing      -> die "bad input(s)"
