import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad

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
        , terminal   = myTerminal
        } `additionalKeys` extraKeys

myTerminal = "urxvt"

----------------------------------------------------------------------
-- Set keys
----------------------------------------------------------------------
myModMask = mod4Mask
extraKeys = [ ((myModMask, xK_l), spawn "slock")
            , ((myModMask, xK_Escape), scratchpadSpawnActionTerminal myTerminal)
            , ((myModMask, xK_Tab), spawn "toggleTouchPad.sh")
            ]

----------------------------------------------------------------------
-- Set workspaces
----------------------------------------------------------------------
myWorkspaces = ["One","Two","Three"]

----------------------------------------------------------------------
-- Setup manage hook
----------------------------------------------------------------------
myManageHook = manageDocks <+> 
               manageScratchPad <+>
               manageHook defaultConfig 

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.9
    w = 0.9
    t = 0  
    l = 0.05

----------------------------------------------------------------------
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

