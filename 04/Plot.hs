module Plot (plotInstructions) where

import Codec.Picture               qualified as Pic
import Control.Monad.IO.Class
import Data.Foldable
import Data.Word
import Graphics.Rasterific         qualified as Raster
import Graphics.Rasterific.Texture qualified as Raster
import Types

type Colour = Pic.PixelRGBA8

fromRGB8 :: Word8 -> Word8 -> Word8 -> Colour
fromRGB8 r g b = Pic.PixelRGBA8 r g b 255

type Plot = Raster.Drawing Colour

white, blue, green, red, yellow, black :: Colour
white = fromRGB8 255 255 255
blue = fromRGB8 0 0 255
green = fromRGB8 0 255 0
red = fromRGB8 255 0 0
yellow = fromRGB8 255 255 0
black = fromRGB8 0 0 0

mkColour :: Color -> Colour
mkColour = \case
  White -> white
  Blue -> blue
  Green -> green
  Red -> red
  Yellow -> yellow

plotOnCanvas
  :: (MonadIO m)
  => Int       -- width
  -> Int       -- height
  -> Colour    -- BG colour
  -> FilePath  -- output path
  -> Plot ()
  -> m ()
plotOnCanvas width height bg outputPath p =
  -- flipping vertically is necessary:
  -- * HTS xml gives coords with origin at bottom left
  -- * Rasterific reads coords with origin at top left
  liftIO . Pic.writePng outputPath . flipVertically $ Raster.renderDrawing width height bg p

flipVertically :: Pic.Pixel pixel => Pic.Image pixel -> Pic.Image pixel
flipVertically img = Pic.generateImage
  pixelFlipped
  (Pic.imageWidth img)
  (Pic.imageHeight img)
  where
    pixelFlipped x y = Pic.pixelAt img x (h - y - 1)
    h = Pic.imageHeight img

plotInstructions :: MonadIO m => Instructions -> FilePath -> m ()
plotInstructions instructions outputPath =
  plotOnCanvas 1000 1000 black outputPath $ traverse_ plot instructions

plot :: Instruction -> Plot ()
plot = \case
  ArcType arc -> plotArc arc
  LineType line -> plotLine line

plotLine :: Line -> Plot ()
plotLine Line {..} = withColour (mkColour lcolor) $
  stroke $ mkLine originPoint endPoint
  where
    mkLine = Raster.Line
    originPoint, endPoint :: Raster.V2 Float
    originPoint = fmap realToFrac $ Raster.V2 xStart yStart
    endPoint = fmap realToFrac $ Raster.V2 xEnd yEnd

withColour :: Colour -> Plot () -> Plot ()
withColour = Raster.withTexture . Raster.uniformTexture

plotArc :: Arc -> Plot ()
plotArc arc@Arc { acolor } = withColour (mkColour acolor) . stroke $ arcToPath arc

arcToPath :: Arc -> Raster.Path
arcToPath arc@Arc {..} = Raster.Path origin False $
  Raster.arcInDirection centre radius' direction tolerance minAngle maxAngle
  where
    origin, centre :: Raster.Point
    centre = realToFrac <$> Raster.V2 xCenter yCenter
    origin = findArcOrigin arc
    radius', tolerance :: Float
    radius' = realToFrac radius
    tolerance = 1  -- what does this do?
    direction = Raster.Forward
    minAngle, maxAngle :: Float
    minAngle = degreesToRadians arcStart
    maxAngle = degreesToRadians (arcStart + arcExtend)

findArcOrigin :: Arc -> Raster.Point
findArcOrigin Arc {..} = Raster.V2 x' y'
  where
    x', y' :: Float
    x' = realToFrac $ xCenter + radius * cos (degreesToRadians arcStart)
    y' = realToFrac $ yCenter + radius * sin (degreesToRadians arcStart)

degreesToRadians :: (Real a, Fractional b, Floating a) => a -> b
degreesToRadians deg = realToFrac $ deg * pi / 180

stroke :: Raster.Geometry geo => geo -> Plot ()
stroke = Raster.stroke strokeWidth Raster.JoinRound (Raster.CapRound, Raster.CapRound)
  where
    strokeWidth :: Float
    strokeWidth = 2

