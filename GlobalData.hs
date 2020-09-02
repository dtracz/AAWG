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


data Position = Cartesian Double Double Double
              | Spherical Double Double Double
    deriving (Show)


_N_CAMS = 2 :: Int

_LEFT_CAM_POS = Cartesian (-1.2) 0.2 0.75
_RIGHT_CAM_POS = Cartesian 0.65 0.2 0.75
_CNN_POS = Cartesian 0 0 0

