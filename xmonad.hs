import Data.Ratio ((%))   
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid   
import XMonad.Layout.IM   
import XMonad.Layout.PerWorkspace   
import XMonad.Layout.Spacing   
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
 
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/nuchs/.xmobarrc"
    xmonad $ myConfig xmproc

----------------------------------------------------------------------
-- Create config
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
extraKeys = [((myModMask, xK_o), spawn "chromium")]

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
  [ className =? "chromium" --> doShift "2:Web"]

----------------------------------------------------------------------
--
-- Setup layout hook
----------------------------------------------------------------------
myLayoutHook = avoidStruts  $  layoutHook defaultConfig


----------------------------------------------------------------------
-- Setup log hook
----------------------------------------------------------------------
myLogHook xmproc = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }

