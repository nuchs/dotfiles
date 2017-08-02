xrdb -merge ~/.Xresources
xmodmap ~/.Xmodmap

xsetroot -cursor_name left_ptr
setxkbmap -layout gb

xautolock -secure \
          -time     10 -locker   "slock" \
          -killtime 10 -killer   "systemctl suspend" &
/usr/bin/dropbox &
urxvtd -f -o -q
/home/nuchs/.dynamic-colors/bin/dynamic-colors switch solarized-dark
feh --bg-scale ~/docs/pics/wallpaper &
exec xmonad
