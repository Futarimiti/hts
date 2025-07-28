module Main where

import Codec.Compression.BZip      qualified as BZip
import Data.ByteString.Char8       qualified as BS8
import Network.HTTP.Client         (Request (..), Response (..), httpLbs, parseRequest)
import Network.HTTP.Client.OpenSSL (newOpenSSLManager)
import Network.HTTP.Types.Header   (hCookie)
import Parse                       (readInstructions)
import Plot                        (plotInstructions)
import System.Environment.Blank    (getEnv)
import System.IO                   (hPutStrLn, stderr)

main :: IO ()
main = do
  Just session <- getEnv "HACK_THIS_SITE_SESSION"
  manager <- newOpenSSLManager
  -- NOTE the ending slash is significant
  basicReq <- parseRequest "https://www.hackthissite.org/missions/prog/4/XML/"
  let request = basicReq
        { requestHeaders = [(hCookie, "HackThisSite=" <> BS8.pack session)] }
  response <- httpLbs request manager
  let
    bzip = responseBody response
    xml = BZip.decompress bzip
  instructions <- readInstructions xml
  plotInstructions instructions "output.png"
  hPutStrLn stderr "Saved results to output.png"
