-------------------------------------------------------------------------------
-- |
-- Module : XMonad.Util.Window.Sorting.Ltbr
-- Copyright : Josh Thibodaux <josh@thibodaux.net>
-- License : BSD3 (see LICENSE)
--
-- Maintainer : Josh Thibodaux <josh@thibodaux.net>
-- Stability : unstable
-- Portability : unportable
--
-- Sorting windows from left-top to bottom-right.
-------------------------------------------------------------------------------
module XMonad.Util.Window.Sorting.Ltbr (
        ltbr
        ) where

import Prelude (Ord (..), Ordering (..), map, (.), Eq (..))
import XMonad.Util.Window (WinRect)
import Data.List (sort)
import Graphics.X11.Xlib.Types (rect_x, rect_y)

data WinWrap = WinWrap WinRect

instance Eq WinWrap where
  (==) (WinWrap a) (WinWrap b) = a == b

instance Ord WinWrap where
  compare (WinWrap (_, ra)) (WinWrap (_, rb)) =
          case (compare (rect_x ra) (rect_x rb), compare (rect_y ra) (rect_y rb)) of
               (EQ, EQ) -> EQ
               (LT, _)  -> LT
               (EQ, LT) -> LT
               _        -> GT

{-| Returs a list of windows sorted by their positions.
 -  Ordering is left top to bottom right.
 -}
ltbr :: [WinRect] -> [WinRect]
ltbr = map (\(WinWrap a) -> a) . sort . map WinWrap
