module Game.Asteroids.Vector(
    rotateV2
  ) where

import Linear

rotateV2 :: Float -> V2 Float -> V2 Float
rotateV2 a (V2 x y) = V2 (x * cos a - y * sin a) (x * sin a + y * cos a)
{-# INLINE rotateV2 #-}
