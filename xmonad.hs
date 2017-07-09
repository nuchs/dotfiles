import XMonad
import XMonad.Actions.Search
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
 
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ myConfig xmproc

----------------------------------------------------------------------
-- Create configs
----------------------------------------------------------------------
myConfig xmproc = docks defaultConfig
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
extraKeys = [ ((myModMask, xK_o),  spawn "qutebrowser")
            , ((0,         xK_F5), promptSearchBrowser defaultXPConfig "qutebrowser" google) 
            , ((0,         xK_F6), selectSearchBrowser "qutebrowser" google)
            , ((myModMask.|.shiftMask, xK_l), spawn "xautolock")
            ]

----------------------------------------------------------------------
-- Set workspaces
----------------------------------------------------------------------
myWorkspaces = ["One","Two","Three","Four"]


----------------------------------------------------------------------
-- Setup manage hook
----------------------------------------------------------------------
myManageHook = manageDocks <+> 
               manageHook defaultConfig


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

