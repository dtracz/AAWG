module TargetCalculator where

import Prelude
import GlobalData
import "AC-Vector" Data.Vector


data Line = Line Vector3 


spher2cart :: SpherPos -> Vector3 
spher2cart (SpherPos r (Direction theta phi)) = let
    x = r * (sin phi) * (cos theta)
    y = r * (sin phi) * (sin theta)
    z = r * (cos phi)
    in Vector3 x y z


cart2spher :: Vector3 -> SpherPos
cart2spher (Vector3 x y z) = let
    r = sqrt $ (x*x) + (y*y) + (z*z)
    theta = if x == 0 then x else atan2 y x
    phi = if z == 0 then (pi / 2) else atan2 (sqrt $ (x*x) + (y*y)) z
    in SpherPos r (Direction theta phi)


getFrameCenter :: Frame -> (Double, Double)
getFrameCenter frame = let
    x = ((realToFrac $ left frame) + (realToFrac $ right frame)) / 2
    y = ((realToFrac $ bottom frame) + (realToFrac $ top frame)) / 2
    in (1-x, 1-y)  -- angles are right to left and top to bottom


imgPos2angle :: (Double, Double) -> (Double, Double)
imgPos2angle (x, y) = let
    x' = _CAM_X_RANGE * (2*x - 1)
    y' = _CAM_Y_RANGE * (2*y - 1)
    in (x', y')


dmod :: Double -> Double -> Double
dmod x y = x - (fromIntegral (floor (x / y)) * y)
        

add :: Direction -> (Double, Double) -> Direction
add (Direction theta phi) (x, y) = let
    theta' = (theta + x) `dmod` (2*pi)
    phi' = (phi + y) `dmod` (2*pi)
    in if phi' > pi then let
        theta'' = (theta' + pi) `dmod` (2*pi)
        phi'' = 2*pi - phi'
        in Direction theta'' phi''
    else 
       Direction theta' phi' 
        

solve_y_aXb :: (Double, Double) -> (Double, Double) -> Maybe (Double, Double)
solve_y_aXb (a1, b1) (a2, b2) =
    if a1 == a2 then
        Nothing
    else let
        x = (b2 - b1) / (a1 - a2)
        y = a1 * x + b1
    in Just (x, y)


targetXY :: Vector3 -> Vector3 -> Double -> Double -> Maybe Vector2
targetXY (Vector3 xl yl _) (Vector3 xr yr _) theta_l theta_r = let
    al = tan theta_l
    bl = yl - (al * xl)
    ar = tan theta_r
    br = yr - (ar * xr)
    in do
        (x, y) <- solve_y_aXb (al, bl) (ar, br)
        return (Vector2 x y)


vlength :: Vector2 -> Double
vlength (Vector2 x y) = sqrt $ (x*x) + (y*y)

ctg :: Floating a => a -> a
ctg x = (cos x) / (sin x)


targetZ :: Vector3 -> Vector3 -> Vector2 -> Double -> Double -> Maybe Double
targetZ (Vector3 xl yl zl) (Vector3 xr yr zr) xy phi_l phi_r = let
    xyl_dist = vlength $ (Vector2 xl yl) - xy
    xyr_dist = vlength $ (Vector2 xr yr) - xy
    z_accL = (ctg phi_l) * (vlength xy) + zl
    z_accR = (ctg phi_r) * (vlength xy) + zr
    in Just $ (z_accL + z_accR) / 2
    


targetPosition :: Frame -> Frame -> Maybe Vector3
targetPosition lc_frame rc_frame = let
    frame2anges = imgPos2angle . getFrameCenter
    (Direction theta_l phi_l) = _LEFT_CAM_DIR `add` (frame2anges lc_frame)
    (Direction theta_r phi_r) = _RIGHT_CAM_DIR `add` (frame2anges rc_frame)
    in do
        (Vector2 x y) <- targetXY _LEFT_CAM_POS _RIGHT_CAM_POS theta_l theta_r
        z <- targetZ _LEFT_CAM_POS _RIGHT_CAM_POS (Vector2 x y) phi_l phi_r
        return (Vector3 x y z)
    
    



