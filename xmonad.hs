import XMonad

main = do
  xmonad $ defaultConfig
    { modMask= superKey
    , terminal = "urxvt"
    }

superKey = mod4Mask
