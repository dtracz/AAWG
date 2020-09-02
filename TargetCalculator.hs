module TargetCalculator where

import Prelude
import GlobalData



spher2cart :: Position -> Position
spher2cart (Spherical r theta phi) = let
    x = r * (sin phi) * (cos theta)
    y = r * (sin phi) * (sin theta)
    z = r * (cos phi)
    in Cartesian x y z


cart2spher :: Position -> Position
cart2spher (Cartesian x y z) = let
    r = sqrt $ (x*x) + (y*y) + (z*z)
    theta = if x == 0 then x else atan2 y x
    phi = if z == 0 then (pi / 2) else atan2 (sqrt $ (x*x) + (y*y)) z
    in Spherical r theta phi



