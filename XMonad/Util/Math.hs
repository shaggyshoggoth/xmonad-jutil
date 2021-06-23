-------------------------------------------------------------------------------
-- |
-- Module : XMonad.Util.Math
-- Copyright : Josh Thibodaux <josh@thibodaux.net>
-- License : BSD3 (see LICENSE)
--
-- Maintainer : Josh Thibodaux <josh@thibodaux.net>
-- Stability : unstable
-- Portability : unportable
--
-- Simple geometry utils.
-------------------------------------------------------------------------------
module XMonad.Util.Math (
    centerOf
) where

import Graphics.X11.Xlib (Rectangle(..)
                         ,Position
                         )

{-| Finds the approximate center of a rectangle.
 -}
centerOf :: Rectangle -> (Position, Position)
centerOf r = (rect_x r + fromIntegral (rect_width r) `div` 2,
              rect_y r + fromIntegral (rect_height r) `div` 2)
