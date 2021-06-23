-------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Actions.EasyNav
-- Copyright   : Josh Thibodaux <josh@thibodaux.net>
-- License     : BSD3 (see LICENSE)
--
-- Maintainer  : Josh Thibodaux <josh@thibodaux.net>
-- Stability   : unstable
-- Portability : unportable
--
-- EasyNav displays a letter in the center of every visible window. Typing that
-- letter warps the mouse and transfers focus to that window.
-------------------------------------------------------------------------------
module XMonad.Actions.EasyNav (
        -- * Usage
        -- $usage
        --
        EasyNavConfig(..)
                              , def
                              , easynav
                              ) where

import Control.Monad (join)
import qualified Data.Map.Strict as M
import Data.Ratio ((%))
import Graphics.X11.Xlib.Extras (currentTime)
import XMonad
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Prompt (mkUnmanagedWindow)
import XMonad.StackSet (focusWindow,screenDetail,current)
import XMonad.Util.Font (Align(AlignCenter)
                        , initXMF
                        , releaseXMF
                        , textExtentsXMF
                        , textWidthXMF
                        , XMonadFont)
import XMonad.Util.Math
import XMonad.Util.Window
import XMonad.Util.XUtils (createNewWindow
                          , deleteWindows
                          , fi
                          , showWindow
                          , paintAndWrite)

data EasyNavConfig =
        ENC { font :: String -- ^ Font name
            , fg :: String -- ^ Foreground color
            , bg :: String -- ^ Background color
            , identifiers :: [Char] -- ^ What to label the windows (FIXME: allow multi-char?)
            , sorting :: [WinRect] -> [WinRect] -- ^ How to sort the windows for labeling
            }

instance Default EasyNavConfig where
  def =
          ENC { font = "-misc-fixed-*-*-*-*-40-*-*-*-*-*-*-*"
              , fg = "white"
              , bg = "black"
              , identifiers = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
              , sorting = id
              }


{-| Starts easy navigation mode.
 -}
easynav ::  EasyNavConfig
        -> X ()
easynav c = do
        f <- initXMF (font c)
        (_, t) <- withWindowSet navigableWindows
        let winids = zip (identifiers c) (sorting c t)
        idwins <- mapM (\(i, (_, r)) -> displayIdentifier c f [i] r) winids
        releaseXMF f
        bracketInput (enKeyHandler (idwins, M.fromList winids))

{- | Grabs mouse and keyboard, performs an action and returns control.
-}
bracketInput :: X () -> X ()
bracketInput h =
        withDisplay $ \dpy -> do
                rootw <- asks theRoot
                scr <- gets $ screenRect . screenDetail . current . windowset
                win <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
                                (rect_x scr) (rect_y scr) (rect_width scr) (rect_height scr)
                liftIO $ mapWindow dpy win
                liftIO $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
                io $ grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
                io $ grabPointer dpy win True buttonReleaseMask grabModeAsync grabModeAsync none none currentTime
                h
                liftIO $ unmapWindow dpy win
                liftIO $ destroyWindow dpy win
                liftIO $ ungrabPointer dpy currentTime
                liftIO $ sync dpy False

{- | Handle keyboard events and switch to window selected.
-}
enKeyHandler :: ([Window], M.Map Char (Window, Rectangle)) -> X ()
enKeyHandler (iw, m) = withDisplay $ \d -> join $ liftIO $ allocaXEvent $ \e -> do
        maskEvent d (exposureMask .|. keyPressMask .|. buttonReleaseMask) e
        ev <- getEvent e
        if ev_event_type ev == keyPress
           then do (_, s) <- lookupString $ asKeyEvent e
                   case (length s, M.lookup (head s) m) of
                        (0, _) -> return $ deleteWindows iw
                        (_, Just (w, _)) -> return $ switchToWindow w >> deleteWindows iw
                        _ -> return $ deleteWindows iw
           else return $ enKeyHandler (iw, m)

{- | Switch focus and mouse pointer to a given window.
-}
switchToWindow :: Window -> X ()
switchToWindow w =
        do windows . focusWindow $ w
           warpToWindow (1%2) (1%2)

{- | Display a character in the middle of a window.
-}
displayIdentifier :: EasyNavConfig
                  -> XMonadFont
                  -> String
                  -> Rectangle
                  -> X Window
displayIdentifier c f s r = do
        d <- asks display
        width <- textWidthXMF d f s
        (as, ds) <- textExtentsXMF f s
        let height = as + ds
            (cx, cy) = centerOf r
            x = cx - fi width `div` 2
            y = cy - height `div` 2
        w <- createNewWindow (Rectangle x y (fi width) (fi height)) Nothing "" True
        showWindow w
        paintAndWrite w f (fi width) (fi height) 0 (bg c) ""
                      (fg c) (bg c) [AlignCenter] [s]
        return w
