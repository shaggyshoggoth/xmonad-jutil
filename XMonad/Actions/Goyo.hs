-------------------------------------------------------------------------------
-- |
-- Module : XMonad.Actions.Goyo
-- Copyright : Josh Thibodaux <josh@thibodaux.net>
-- License : BSD3 (see LICENSE)
--
-- Maintainer : Josh Thibodaux <josh@thibodaux.net>
-- Stability : unstable
-- Portability : unportable
--
-- Goyo is intended to work with the vim Goyo plugin. When activated
-- the active window is made fullscreen and all other monitors are blanked.
-------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Actions.Goyo (
        goyoToggle
        ) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils (createNewWindow
                          , showWindow
                          , paintWindow)

data GoyoState = GoyoState [Window] (Maybe (Layout Window))
  deriving Typeable

instance ExtensionClass GoyoState where
  initialValue = GoyoState [] Nothing

{-| Toggles Goyo window mode.
 -}
goyoToggle :: X ()
goyoToggle = do
        gs <- XS.get
        case gs of
             GoyoState [] _ -> goyoOn
             GoyoState ws l -> goyoOff ws l

{-| Turns on Goyo window mode.
 -}
goyoOn :: X ()
goyoOn = do
        rs <- inactiveScreenRects
        ws <- mapM blankScreen rs
        l  <- activeScreenLayout
        setLayout (Layout Full)
        XS.put (GoyoState ws (Just l))

{-| Turns off Goyo window mode.
 -}
goyoOff :: [Window] -> Maybe (Layout Window) -> X ()
goyoOff ws l = do
        XS.put (GoyoState [] Nothing)
        mapM_ unblankScreen ws
        maybe (return ()) setLayout l

{-| Returns the layout of the currently active screen.
 -}
activeScreenLayout :: X (Layout Window)
activeScreenLayout =
        (W.layout . W.workspace) <$> withWindowSet (return . W.current)

{-| Returns the list of inactive screens.
 -}
inactiveScreenRects :: X [Rectangle]
inactiveScreenRects =
        map (screenRect . W.screenDetail) <$> withWindowSet (return . W.visible)

{-| Blanks a screen by painting a black window over all other windows.
 -}
blankScreen :: Rectangle
            -> X Window
blankScreen r = do
        w <- createNewWindow r Nothing "" True
        let width = rect_width r
            height = rect_height r
        showWindow w
        paintWindow w width height 0
                    "black" "black"
        return w

{-| Removes the blanking window.
 -}
unblankScreen :: Window
              -> X ()
unblankScreen win =
        withDisplay $ \dpy -> do
                liftIO $ unmapWindow dpy win
                liftIO $ destroyWindow dpy win
