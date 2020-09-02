module Net where

import Foreign
import Foreign.Ptr
import Foreign.C
import Foreign.C.String
import Foreign.Storable

import GlobalData



instance Storable Frame where
    alignment _ = 8
    sizeOf _    = 24 --40
    peek ptr    = Frame
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 4
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 12
        <*> peekByteOff ptr 16
        <*> peekByteOff ptr 20
        -- <*> peekByteOff ptr 24
        -- <*> peekByteOff ptr 32
    -- poke ptr (Frame label prob left botom right top frame getArea) = do
    poke ptr (Frame label prob left botom right top) = do
        pokeByteOff ptr 0  label
        pokeByteOff ptr 4  prob
        pokeByteOff ptr 8  left
        pokeByteOff ptr 12 botom
        pokeByteOff ptr 16 right
        pokeByteOff ptr 20 top
        -- pokeByteOff ptr 20 frame
        -- pokeByteOff ptr 20 getArea

data NetRunner

foreign import ccall "_ZN9NetRunner12getNetRunnerEPKc"
               c_getNetRunner :: CString -> IO (Ptr NetRunner)
foreign import ccall "_ZN9NetRunner7pushImgEPKc"
               c_pushImg :: Ptr NetRunner -> CString -> IO CInt
foreign import ccall "_ZN9NetRunner6runNetEP5Frame"
               c_runNet :: Ptr NetRunner -> Ptr Frame -> IO CInt

getNetRunner :: IO (Ptr NetRunner)
getNetRunner = do
    modelPath <- newCString  "./net/traced_mobilenet_v2-ssd.pt"
    c_getNetRunner modelPath

pushImg :: String -> IO Int
pushImg fname = do
    cs_fname <- newCString fname
    netRunner <- getNetRunner
    n <- c_pushImg netRunner cs_fname
    return $ fromInteger $ toInteger n

runNet :: Ptr Frame -> IO Int
runNet framesPtr = do
    netRunner <- getNetRunner
    n <- c_runNet netRunner framesPtr
    return $ fromInteger $ toInteger n


memToList :: Storable a => [a] -> Ptr a -> Int -> Int -> IO [a]
memToList lst ptr aSize 0 = return $ reverse lst
memToList lst ptr aSize n = do
    item <- peek ptr
    let ptr' = plusPtr ptr aSize
    memToList (item:lst) ptr' aSize $ n-1

getFrameList :: Int -> IO [Frame]
getFrameList nFrames = do
    framesPtr <- mallocBytes (sizeOfFrame * nFrames)
    nFrames' <- runNet $ castPtr framesPtr
    framesLst <- memToList [] framesPtr sizeOfFrame nFrames'
    free framesPtr
    return framesLst


detectTarget :: String -> IO [Frame]
detectTarget fname = do
    nStored <- pushImg fname
    if nStored == _N_CAMS then
        getFrameList nStored
    else
        return []
    

