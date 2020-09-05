{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.ByteString.Base64
import Data.Text.Internal
import Codec.Picture
import qualified Network.HTTP.Client as Client

import Net
import GlobalData 



main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app
 
app req respond = do
    bs <- getImgBS req
    let imgName = "test_name.jpg"
    res <- saveImage imgName bs
    frames <- detectTarget imgName
    putStrLn $ res
    putStrLn $ show frames
    res <- respond $ respConst res
    return res


getLgh (KnownLength kl) = fromIntegral kl

getImgBS :: Request -> IO BU.ByteString
getImgBS req = do
    let leftLgh = getLgh $ requestBodyLength req
    bsl <- getImgBS' [] req leftLgh
    return $ mconcat $ reverse bsl 

getImgBS' :: [BU.ByteString] -> Request -> Int -> IO [BU.ByteString]
getImgBS' bsl req 0 = return bsl
getImgBS' bsl req leftLgh = do
    bs <- getRequestBodyChunk req
    getImgBS' (bs:bsl) req (leftLgh - (BU.length bs))
    

saveImage :: String -> BU.ByteString -> IO String
saveImage imgName bs = do
    let bs' = decodeLenient bs
    case decodeImage bs' of
        Left msg -> return msg 
        Right img -> saveImage' imgName img

saveImage' :: String -> DynamicImage -> IO String
saveImage' imgName img = do
    saveJpgImage 100 imgName img
    return "OK"


-- bsbConst :: Show a => a -> Data.ByteString.Builder.Internal.Builder
bsbConst x = mconcat $ map copyByteString [ BU.fromString $ show x ]

respConst :: Show a => a -> Response
respConst x = responseBuilder status200 [("Content-Type", "text/html")] $ bsbConst x


constructOrder :: String -> String -> Int -> IO Client.Request
constructOrder content hostname port = do
    let body = Client.RequestBodyBS $ BU.fromString content
    req <- Client.parseRequest $ "POST http://" ++ hostname
    return (req {Client.port = port, Client.requestBody = body})
        
    
sendPost :: SpherPos -> IO ()
sendPost targetPos = do
    manager <- Client.newManager Client.defaultManagerSettings
    req <- constructOrder (show targetPos) "localhost" 3100
    response <- Client.httpLbs req manager
    let obj = Client.responseBody response
    putStrLn $ show obj
    

