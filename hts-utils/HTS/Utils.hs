module HTS.Utils (fetch) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Char8       qualified as BS8
import Data.ByteString.Lazy
import Network.HTTP.Client         (Request (..), Response (..), httpLbs, parseRequest)
import Network.HTTP.Client.OpenSSL (newOpenSSLManager)
import Network.HTTP.Types.Header   (hCookie)
import System.Environment.Blank    (getEnv)

-- | fetch a resource on HTS given url, return the result as lazy BS.
-- NOTE: the ending slash in the url will be significant
fetch :: (MonadThrow m, MonadIO m, MonadFail m) => FilePath -> m ByteString
fetch url = do
  Just session <- liftIO $ getEnv "HACK_THIS_SITE_SESSION"
  manager <- newOpenSSLManager
  basicReq <- parseRequest url
  let request = basicReq
        { requestHeaders = [(hCookie, "HackThisSite=" <> BS8.pack session)] }
  response <- liftIO $ httpLbs request manager
  pure $ responseBody response
