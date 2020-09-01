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

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app
 
app req respond = do
    bs <- getImgBS req
    res <- saveImage bs
    putStrLn $ res
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
    

saveImage :: BU.ByteString -> IO String
saveImage bs = do
    let bs' = decodeLenient bs
    case decodeImage bs' of
        Left msg -> return msg 
        Right img -> saveImage' img

saveImage' :: DynamicImage -> IO String
saveImage' img = do
    saveJpgImage 100 "test.jpg" img
    return "OK"


-- bsbConst :: Show a => a -> Data.ByteString.Builder.Internal.Builder
bsbConst x = mconcat $ map copyByteString [ BU.fromString $ show x ]

respConst :: Show a => a -> Response
respConst x = responseBuilder status200 [("Content-Type", "text/html")] $ bsbConst x


