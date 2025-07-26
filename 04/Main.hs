module Main where

import Parse     (readInstructions)
import Paths_l04 (getDataFileName)
import Plot      (plotInstructions)

main :: IO ()
main = do
  xml <- getDataFileName "plotMe.xml"
  instructions <- readInstructions xml
  plotInstructions instructions "output.png"

