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
 
app req respond =
    do
        res <- respond $ reaction $ action (showText $ head (pathInfo req))
        putStrLn $ "!!!!!!!!"
        return res
 
action :: String -> String
action str = ">" ++ str ++ "<" 


getAns x = mconcat $ map copyByteString [ BU.fromString $ show x ]

reaction :: Show a => a -> Response
reaction x = responseBuilder status200 [("Content-Type", "text/html")] $ getAns x


