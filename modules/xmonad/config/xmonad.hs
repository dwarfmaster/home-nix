import XMonad hiding ((|||))
import XMonad.Layout hiding ((|||))
import XMonad.Operations
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedWindows (getName)
import System.IO
import System.Exit
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86

import XMonad.Layout.Accordion
import XMonad.Layout.AutoMaster
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.Cross
import XMonad.Layout.Drawer
import XMonad.Layout.MagicFocus -- Move focused window to master
import XMonad.Layout.Magnifier  -- Increase size of the focused window
import XMonad.Layout.Master
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders -- noBorders transformer
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace -- Configure layouts on per-workspace basis
import XMonad.Layout.Roledex -- Better than circle
import XMonad.Layout.Spiral
import XMonad.Actions.RotSlaves
import XMonad.Layout.ToggleLayouts
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace -- An alternative to workspaces
import XMonad.Actions.UpdatePointer
import XMonad.Layout.Column
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed

-- Interesting layouts :
--  - Circle
--  - mastered (1/100) 0.6 Accordion
--  - Roledex
--  - spiral (6/7)

modkey = mod4Mask

mlayout = Full ||| tiled ||| Circle
 where tiled   = renamed [Replace "Tiled"] inter
       inter   = spacingRaw False (mkBord 0) False (mkBord 10) True
               $ mastered delta ratio $ toggleLayouts Accordion $ Column 1
       ratio   = 0.5 -- Change to golden ratio
       delta   = 3/100
       mkBord  = \n -> Border n n n n

mworkspaces = ["r1", "l1", "l4", "l3", "l2", "music", "misc", "r2", "r3", "r4"]
wkkeys = [xK_j, xK_f, xK_q, xK_s, xK_d, xK_g, xK_h, xK_k, xK_l, xK_m]

rofi_clip = "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'"
rofi_calc = "rofi -plugin-path '/home/luc/.nix-profile/share/rofi/plugins/' -show calc -modi calc -no-show-match -no-sort -calc-command \"echo '{result}' | xclip\""

keybinds = M.fromList $ foldl kwk
         [ ((modkey, xK_Return),           spawn "st")
         , ((modkey, xK_r),                spawn "rofi -show run")
         , ((modkey, xK_v),                spawn rofi_clip)
         , ((modkey, xK_c),                spawn rofi_calc)
         -- TODO : , ((modkey, xK_c), spawn "") calcul utility
         , ((modkey, xK_p),                spawn "scrlock")
         , ((modkey .|. shiftMask, xK_n),  io (exitWith ExitSuccess))
         , ((modkey, xK_n),                spawn "xmonad --recompile && xmonad --restart")
         -- Client focus control
         , ((modkey, xK_x),                         kill)
         , ((modkey, xK_Tab),                       windows W.focusDown)
         , ((modkey .|. shiftMask, xK_Tab),         rotSlavesDown)
         , ((modkey, xK_twosuperior),               windows W.focusUp)
         , ((modkey .|. shiftMask, xK_twosuperior), rotSlavesUp)
         , ((modkey, xK_i),                         windows W.focusDown)
         , ((modkey, xK_o),                         windows W.focusUp)
         , ((modkey .|. shiftMask, xK_Return),      windows W.swapMaster)
         , ((modkey, xK_space),                     windows W.focusMaster)
         -- Screen focus control
         , ((modkey, xK_comma),                   prevScreen)
         , ((modkey, xK_semicolon),               nextScreen)
         , ((modkey .|. shiftMask, xK_comma),     shiftPrevScreen)
         , ((modkey .|. shiftMask, xK_semicolon), shiftNextScreen)
         , ((modkey .|. shiftMask, xK_o),         shiftNextScreen >> nextScreen)
         -- Layout control
         , ((modkey, xK_a), sendMessage $ JumpToLayout "Full")
         , ((modkey .|. shiftMask, xK_z), sendMessage ToggleLayout)
         , ((modkey, xK_z), sendMessage $ JumpToLayout "Tiled")
         , ((modkey, xK_e), sendMessage $ JumpToLayout "Circle")
         -- Audio control
         , ((modkey, xF86XK_AudioRaiseVolume), raiseVolume 5 >> return ())
         , ((modkey, xF86XK_AudioLowerVolume), lowerVolume 5 >> return ())
         , ((modkey, xF86XK_AudioMute),        toggleMute    >> return ())
         ] $ zip mworkspaces wkkeys
 where kwk l (wk,k) = l ++
         [ ((modkey, k),               windows $ W.greedyView wk)
         , ((modkey .|. shiftMask, k), windows $ W.shift wk)]

loghk xmp = dynamicLogWithPP xmobarPP
          { ppOutput  = hPutStrLn xmp
          , ppTitle   = xmobarColor "#d7c8bc" "" . shorten 100
          , ppLayout  = const "" -- to disable layout info on xmobar
          , ppSep     = " | "
          , ppCurrent = xmobarColor "#ca7f32" ""
          , ppVisible = xmobarColor "#e0ac16" ""
          , ppHidden  = xmobarColor "#d7c8bc" ""
          }

mconfig xmp1 = docks $ def
        { borderWidth        = 2
        , terminal           = "st"
        , normalBorderColor  = "#b4a490"
        , focusedBorderColor = "#6eb958"
        , modMask            = modkey
        , layoutHook         = avoidStruts $ mlayout
        , workspaces         = mworkspaces
        , keys               = const keybinds
        , manageHook         = manageDocks <+> manageHook def
        , logHook            = loghk xmp1 >> updatePointer (0.5,0.5) (0,0)
        }

main = do xmp1 <- spawnPipe "/usr/bin/env xmobar /home/luc/.xmonad/xmobarrc"
          xmonad $ mconfig xmp1

