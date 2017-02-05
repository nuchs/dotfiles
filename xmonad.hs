import Data.Ratio ((%))   
import XMonad
import XMonad.Actions.Search
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid   
import XMonad.Layout.IM   
import XMonad.Layout.PerWorkspace   
import XMonad.Layout.Spacing   
import XMonad.Prompt
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
 
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/nuchs/.xmobarrc"
    xmonad $ myConfig xmproc

----------------------------------------------------------------------
-- Create configs
----------------------------------------------------------------------
myConfig xmproc = defaultConfig
        { manageHook = myManageHook
        , layoutHook = myLayoutHook
        , logHook    = myLogHook xmproc
        , workspaces = myWorkspaces
        , modMask    = myModMask
        , terminal   = "urxvt"
        } `additionalKeys` extraKeys

----------------------------------------------------------------------
-- Set keys
----------------------------------------------------------------------
myModMask = mod4Mask
extraKeys = [ ((myModMask, xK_o),  spawn "chromium")
            , ((0,         xK_F5), selectSearchBrowser "chromium" google)
            , ((0,         xK_F6), promptSearchBrowser defaultXPConfig "chromium" google) 
            , ((0,         xK_F7), promptSearchBrowser defaultXPConfig "chromium" maps) 
            ]

----------------------------------------------------------------------
-- Set workspaces
----------------------------------------------------------------------
myWorkspaces = ["1:Main","2:Web","3:Chat","4:FarAway"]


----------------------------------------------------------------------
-- Setup manage hook
----------------------------------------------------------------------
myManageHook = manageDocks <+> 
               myTriggers  <+> 
               manageHook defaultConfig

myTriggers = composeAll
  [ className =? "Chromium" --> doShift "2:Web"]

----------------------------------------------------------------------
--
-- Setup layout hook
----------------------------------------------------------------------
myLayoutHook = avoidStruts $ layoutHook defaultConfig


----------------------------------------------------------------------
-- Setup log hook
----------------------------------------------------------------------
myLogHook xmproc = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }

