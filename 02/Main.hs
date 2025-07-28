module Main where

import Codec.Picture        qualified as Pic
import Codec.Picture.Types  qualified as Pic
import Control.Applicative
import Control.Monad        (guard)
import Data.ByteString.Lazy qualified as BSL
import Data.Char
import HTS.Utils            qualified as HTS
import Morse qualified
import System.Hclip
import System.IO            (hPutStrLn, stderr)

main :: IO ()
main = do
  image <- fetchImage
  numbers <- locateWhitePixels image
  let cyphertext = map chr $ diffs (0 : numbers)
  Just decoded <- pure $ Morse.decode cyphertext
  putStrLn decoded
  hPutStrLn stderr "(also saved to clipboard)"
  setClipboard decoded

flipVertically :: Pic.Pixel pixel => Pic.Image pixel -> Pic.Image pixel
flipVertically img = Pic.generateImage pixelFlipped (Pic.imageWidth img) (Pic.imageHeight img)
  where
    h = Pic.imageHeight img
    pixelFlipped x y = Pic.pixelAt img x (h - y - 1)

-- returned pixels in an order of from left to right, top to bottom
locateWhitePixels :: (Alternative m, Monad m) => Pic.Image Pic.PixelRGB8 -> m [Int]
locateWhitePixels = fmap reverse . flip Pic.pixelFoldM [] \acc x y pixel -> do
  guard $ x <= 99
  pure $ case pixel of
    Pic.PixelRGB8 255 255 255 -> x + y * 100 : acc
    _                         -> acc

diffs :: Num a => [a] -> [a]
diffs = zipWith diff <*> tail
  where
    diff :: Num a => a -> a -> a
    diff = (abs .) . (-)

fetchImage :: IO (Pic.Image Pic.PixelRGB8)
fetchImage = do
  imageLBS <- HTS.fetch "https://www.hackthissite.org/missions/prog/2/PNG/"
  Right dynImage <- pure . Pic.decodeImage $ BSL.toStrict imageLBS
  pure $ Pic.convertRGB8 dynImage
