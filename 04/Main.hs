module Main where

import Codec.Compression.BZip qualified as BZip
import HTS.Utils              qualified as HTS
import Parse                  (readInstructions)
import Plot                   (plotInstructions)
import System.IO              (hPutStrLn, stderr)

main :: IO ()
main = do
  bzip <- HTS.fetch "https://www.hackthissite.org/missions/prog/4/XML/"
  let xml = BZip.decompress bzip
  instructions <- readInstructions xml
  plotInstructions instructions "output.png"
  hPutStrLn stderr "Saved results to output.png"
