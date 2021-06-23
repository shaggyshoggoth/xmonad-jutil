-------------------------------------------------------------------------------
-- |
-- Module : XMonad.Util.Window
-- Copyright : Josh Thibodaux <josh@thibodaux.net>
-- License : BSD3 (see LICENSE)
--
-- Maintainer : Josh Thibodaux <josh@thibodaux.net>
-- Stability : unstable
-- Portability : unportable
--
-- Simple window info utils.
-------------------------------------------------------------------------------
module XMonad.Util.Window (
  navigableWindows,
  WinRect
) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import XMonad
import qualified XMonad.StackSet as W

type WinRect = (Window, Rectangle)

{-| Returns the list of windows on the currently visible workspaces. Result
    is (Floating Windows, Tiled Windows).
-}
navigableWindows :: WindowSet -> X ([WinRect], [WinRect])
navigableWindows winset = L.partition (\(win, _) -> M.member win (W.floating winset))
                        . catMaybes
                        . concat
                        <$>
                        (mapM (mapM maybeWindowRect
                               . W.integrate'
                               . W.stack
                               . W.workspace
                              )
                              . W.screens
                        ) winset
    where maybeWindowRect win = do r <- windowRect win
                                   return ((,) win <$> r)

-- | Returns the current rectangle of the given window, Nothing if the window isn't mapped.
windowRect :: Window -> X (Maybe Rectangle)
windowRect win = withDisplay $ \dpy -> do
        mp <- isMapped win
        if mp then do (_, x, y, w, h, bw, _) <- io $ getGeometry dpy win
                      return $ Just $ Rectangle x y (w + 2 * bw) (h + 2 * bw)
                      `catchX` return Nothing
              else return Nothing

-- | Determines whether a given window is mapped.
isMapped :: Window -> X Bool
isMapped win = withDisplay
             $ \dpy -> io
             $ (waIsUnmapped /=)
             . wa_map_state
             <$> getWindowAttributes dpy win
