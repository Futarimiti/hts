{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Applicative
import Data.Functor
import GHC.Generics                    (Generic)
import Text.ParserCombinators.ReadP    hiding (get)
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read

data Color = Blue | Green | Red | Yellow | White
  deriving (Show, Eq, Generic)

instance Read Color where
  readPrec = ReadPrec.lift $
    string "blue" $> Blue
    <|> string "green" $> Green
    <|> string "red" $> Red
    <|> string "yellow" $> Yellow
    -- <|> string "white" $> White

data Arc = Arc
  { xCenter   :: Double
  , yCenter   :: Double
  , radius    :: Double
  , acolor    :: Color
  , arcStart  :: Double
  , arcExtend :: Double
  } deriving (Show, Eq, Generic)

data Line = Line
  { xStart :: Double
  , yStart :: Double
  , xEnd   :: Double
  , yEnd   :: Double
  , lcolor :: Color
  } deriving (Show, Eq, Generic)

data Instruction
  = ArcType Arc
  | LineType Line
  deriving (Show, Eq, Generic)

type Instructions = [Instruction]
