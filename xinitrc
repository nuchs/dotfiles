xrdb -merge ~/.Xresources
xmodmap ~/.Xmodmap

setxkbmap -layout gb

xautolock -secure \
          -time     10 -locker   "slock" &
/usr/bin/dropbox &
urxvtd -f -o -q
/home/nuchs/.dynamic-colors/bin/dynamic-colors switch solarized-dark
feh --bg-scale ~/docs/pics/wallpaper &
exec xmonad

