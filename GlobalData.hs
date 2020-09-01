module GlobalData where

import Foreign.C



data Frame = Frame 
        { label :: CInt
        , prob  :: Float
        , left  :: Float
        , botom :: Float
        , right :: Float
        , top   :: Float
        -- , frame :: Ptr () -- C++ constructor, ignoring
        -- , getArea :: FunPtr (() -> Float)
        } deriving (Show)

sizeOfFrame = 24 :: Int

-- getFrame label prob left botom right top =
--     Frame label prob left botom right top nullPtr nullPtr

_N_CAMS = 2 :: Int


