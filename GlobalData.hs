module GlobalData where

import Foreign.C
import "AC-Vector" Data.Vector


data Frame = Frame 
        { label :: CInt
        , prob  :: Float
        , left  :: Float
        , bottom :: Float
        , right :: Float
        , top   :: Float
        -- , frame :: Ptr () -- C++ constructor, ignoring
        -- , getArea :: FunPtr (() -> Float)
        } deriving (Show)

sizeOfFrame = 24 :: Int

-- getFrame label prob left botom right top =
--     Frame label prob left botom right top nullPtr nullPtr


data Direction = Direction Scalar Scalar deriving (Show)

data SpherPos = SpherPos Scalar Direction deriving (Show)



_N_CAMS = 2 :: Int

_LEFT_CAM_POS = Vector3 0.2 1.2 0.75
_RIGHT_CAM_POS = Vector3 0.2 (-0.65) 0.75
_CNN_POS = Vector3 0 0 0

_LEFT_CAM_DIR = Direction 0 (pi/2)
_RIGHT_CAM_DIR = Direction 0 (pi/2)

_CAM_X_RANGE = pi/4
_CAM_Y_RANGE = pi/4

_SELF_PORT = 3000 :: Int
_TURRET_HOSTNAME = "localhost"
_TURRET_PORT = 3100 :: Int

_TMP_IMG_PATH = "tmp/"
