-- Configs that this one is based on:
-- https://github.com/madgrid/xmonad-nixOS/blob/master/xmonad/xmonad.hs
-- https://github.com/Forkk/dotfiles/blob/master/.xmonad/xmonad.hs

import XMonad

import XMonad.Actions.Navigation2D
import XMonad.Config
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.FixedColumn
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle

import XMonad.Util.CustomKeys
import XMonad.Util.Run

import Graphics.X11.Types (xK_Print)

import Data.List
import qualified Data.Map as M

main :: IO ()
main = do
  xmonad
  . ewmhFullscreen
  . ewmh
  . withEasySB (statusBarProp xmobarArgs (pure myXmobarPP)) toggleStrutsKey
  $ myConfig
 where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig { modMask = m } = (m, xK_b)

xmobarArgs = "xmobar -d -B " ++ stringed backgroundColor
  ++ " -F " ++ stringed middleColor
  where stringed x = "\"" ++ x ++ "\""

backgroundColor   = "#202020"
middleColor       = "#AEAEAE"
foregroundColor   = "#aaffaa"

myConfig = def
  { borderWidth = 1
  , focusFollowsMouse  = False
  , modMask = mod4Mask
  , terminal = "kitty"
  , layoutHook         = myLayoutHook
  , workspaces         = myWorkspaces
  , keys          = \c -> myKeys c `M.union` keys def c
  }

myWorkspaces = ["1:work", "2:web", "3:msg", "4", "5", "6"]
-- myWorkspaces = withScreens 2 ["1:work", "2:web", "3:msg", "4", "5", "6"]

togglevga = do
  screencount <- countScreens
  if screencount > 1
   then spawn "xrandr --output DP1 --off"
   else spawn "xrandr --output DP1 --mode '2560x1440' --above eDP1"
   -- "xrandr --output VGA1 --auto --right-of LVDS1"

toggleBluetooth = do
  spawn "if [[ $(bluetoothctl show | awk -F': ' '/Powered:/ { print $2 }') == 'no' ]]; \
\    then bluetoothctl power on; \
\    else bluetoothctl power off; \
\  fi"

myKeys (XConfig {modMask = modm}) = M.fromList $
  [ ((0, xK_Print), spawn "maim -s \"/tmp/screenshot-$(date -Iseconds).png\"")
  , ((0, xK_F7), togglevga)
  , ((0, xK_F10), toggleBluetooth)
  -- ((modm , xK_x), spawn "xlock")
  ]

myXmobarPP = def
  { ppCurrent         = pad . xmobarColor foregroundColor  ""
  , ppHidden          = pad . xmobarColor middleColor ""
  , ppHiddenNoWindows = pad . xmobarColor middleColor ""
  , ppLayout          = const ""
  , ppTitle           = const ""
  , ppVisible         = pad . xmobarColor middleColor ""
  , ppWsSep           = " "
  }

myLayoutHook = (avoidStruts (tall ||| GridRatio (4/3) ||| Full ))
  where tall = Tall 1 (3/100) (1/2)

