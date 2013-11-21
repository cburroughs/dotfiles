import XMonad
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Config.Xfce
import XMonad.Config.Gnome
import XMonad.Actions.CycleWindows -- classic alt-tab
import XMonad.Layout.Spiral
import XMonad.Hooks.SetWMName
import Data.Ratio
  
-- todo: gimp, arrow, pidgin, fullscreen

-- on floating: http://hpaste.org/55387
  -- a basic CycleWS setup
  -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-CycleWS.html

--myLayouts =  spiral (1 % 1) ||| tiled ||| Mirror tiled ||| Full

main = xmonad $ gnomeConfig
 { modMask = mod4Mask
 , terminal = "gnome-terminal"
 , focusFollowsMouse = False
 , startupHook = setWMName "LG3D"                       
-- , layoutHook = myLayouts

 }
 `additionalKeysP`
 [
   ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour (good?)
 , ("M-<Left>", prevWS)
 , ("M-<Right>", nextWS)
   
 ,("M-s" , spawn "gnome-terminal")
 ,("M-e" , spawn "emacs")
 , ("M-c" , spawn "chromium")
 , ("M-f" , spawn "iceweasel")
-- , ("M-S-<Left>", shiftToNext)
-- , ("M-S-<Right>", shiftToPrev)

 ]



myKeys = [
  ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour

 -- , ((modm,               xK_Down),  nextWS)
 -- , ((modm,               xK_Up),    prevWS)
 -- , ((modm .|. shiftMask, xK_Down),  shiftToNext)
 -- , ((modm .|. shiftMask, xK_Up),    shiftToPrev)
 -- , ((modm,               xK_Right), nextScreen)
 -- , ((modm,               xK_Left),  prevScreen)
 -- , ((modm .|. shiftMask, xK_Right), shiftNextScreen)
 -- , ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
 -- , ((modm,               xK_z),     toggleWS)
  ]
