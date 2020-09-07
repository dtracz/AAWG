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
import Control.Monad.Trans.Maybe 
import Control.Concurrent

import Net
import GlobalData 
import TargetCalculator



--SERVER------------------------------------------------------------------------


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


getHeader :: Eq t => [(t, BU.ByteString)] -> t -> Maybe String
getHeader [] target = Nothing
getHeader ((key,val):hds) target
    | key == target = Just (BU.toString val)
    | otherwise = getHeader hds target
    
getImgName :: Request -> Maybe String
getImgName req = do
    let getHeader' = getHeader $ requestHeaders req
    camside <- getHeader' "camside"
    timestamp <- getHeader' "timestamp"
    return $ _TMP_IMG_PATH ++ camside ++ "_" ++ timestamp ++ ".jpg"


--RESPONDER---------------------------------------------------------------------


constructOrder :: String -> String -> Int -> IO Client.Request
constructOrder content hostname port = do
    let body = Client.RequestBodyBS $ BU.fromString content
    req <- Client.parseRequest $ "POST http://" ++ hostname
    return (req {Client.port = port, Client.requestBody = body})
        
    
sendPost :: SpherPos -> IO String
sendPost targetPos = do
    manager <- Client.newManager Client.defaultManagerSettings
    req <- constructOrder (show targetPos) _TURRET_HOSTNAME _TURRET_PORT
    response <- Client.httpLbs req manager
    let obj = Client.responseBody response
    return $ show obj
    


--MAIN--------------------------------------------------------------------------


main :: IO ()
main = do
    let port = _SELF_PORT
    putStrLn $ "Listening on port " ++ show port
    run port app


app :: Request -> (Response -> IO b) -> IO b
app req respond = do
    let imgName = getImgName req
    case getImgName req of
        Nothing -> respond $ respConst "headers unfound"
        Just imgName -> do
            bs <- getImgBS req
            res <- saveImage imgName bs
            thid <- forkIO $ action imgName
            respond $ respConst res


lst2tpl :: [Frame] -> (Frame, Frame)
lst2tpl (x:y:_) = (x,y)
lst2tpl _ = (Frame 0 0 0 0 0 0, Frame 0 0 0 0 0 0)


action :: String -> IO ()
action imgName = do
    frames <- detectTarget imgName
    let spherPos = ((fmap cart2spher) . targetPosition . lst2tpl) =<< frames
    mstr <- sequence $ fmap sendPost spherPos 
    -- sequence $ fmap putStrLn mstr
    return ()
    

