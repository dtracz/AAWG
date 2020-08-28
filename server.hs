{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.Text.Internal
 
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app
 
app req respond = do
    -- let reqStr = showText $ head (pathInfo req)
    iobs <- getRequestBodyChunk req
    let body = BU.toString iobs
    putStrLn $ ">>" ++ body ++ "<<"
    res <- respond $ respConst "OK"
    return res
 

-- bsbConst :: Show a => a -> Data.ByteString.Builder.Internal.Builder
bsbConst x = mconcat $ map copyByteString [ BU.fromString $ show x ]

respConst :: Show a => a -> Response
respConst x = responseBuilder status200 [("Content-Type", "text/html")] $ bsbConst x


