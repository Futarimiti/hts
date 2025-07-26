module Parse (readInstructions) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.CaseInsensitive   (CI)
import Data.Maybe
import Data.Text              (Text)
import Data.Text              qualified as Text
import Text.Read              (readMaybe)
import Text.XML               qualified as XML
import Text.XML.Lens          qualified as XML
import Types

readInstructions
  :: (MonadIO m, Alternative m, MonadThrow m) => FilePath -> m Instructions
readInstructions = toDocument >=> parseDrawingInstructions

toDocument :: MonadIO io => FilePath -> io XML.Document
toDocument = liftIO . XML.readFile XML.def

parseDrawingInstructions
  :: (Alternative m, MonadThrow m) => XML.Document -> m Instructions
parseDrawingInstructions doc = do
  instructionElements <- getInstructionElements doc
  traverse parseDrawingInstruction instructionElements

data NoParseException = NoParseException
  deriving (Show, Eq)

instance Exception NoParseException

parseDrawingInstruction
  :: (Alternative m, MonadThrow m) => XML.Element -> m Instruction
parseDrawingInstruction el =
  parseAsArc el <|> parseAsLine el <|> throwM NoParseException

parseAsLine :: forall m. (Alternative m, Monad m) => XML.Element -> m Instruction
parseAsLine el = do
  xStart <- get "XStart" el
  yStart <- get "YStart" el
  xEnd   <- get "XEnd" el
  yEnd   <- get "YEnd" el
  lcolor <- get "Color" el <|> pure White
  pure . LineType $ Line {..}
  where
    get :: Read a => CI Text -> XML.Element -> m a
    get = getSubNode "Line"

getSubNode
  :: (Read a, Alternative m) => CI Text -> CI Text -> XML.Element -> m a
getSubNode root name el = case el ^? XML.named root ... XML.named name . XML.nodes of
  Just [XML.NodeContent content] -> readA (Text.unpack content)
  _                              -> empty

parseAsArc :: forall m. (Monad m, Alternative m) => XML.Element -> m Instruction
parseAsArc el = do
  xCenter <- get "XCenter" el
  yCenter <- get "YCenter" el
  radius <- get "Radius" el
  acolor <- get "Color" el <|> pure White
  arcStart <- get "ArcStart" el
  arcExtend <- get "ArcExtend" el
  pure . ArcType $ Arc {..}
  where
    get :: Read a => CI Text -> XML.Element -> m a
    get = getSubNode "Arc"

-- @readMaybe@ generalise to every instance of @Alternative@
readA :: (Read a, Alternative m) => String -> m a
readA = readMaybe >>> \case
  Just a -> pure a
  Nothing -> empty

getInstructionElements :: Applicative m => XML.Document -> m [XML.Element]
getInstructionElements = pure . mapMaybe (preview XML._Element) .
  view (XML.root . XML.named "ppcPlot" . XML.nodes)

