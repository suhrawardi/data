module HsDownload (downloadData) where

import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit as C
import Data.Conduit.Binary (sinkFile)
import Data.Maybe
import Network.HTTP.Conduit
import Network.URI
import System.IO

downloadData :: String -> FilePath -> IO ()
downloadData url path = do
  request <- parseRequest url
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager
    responseBody response C.$$+- sinkFile path
