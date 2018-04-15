xrdb -merge ~/.Xresources
xmodmap ~/.Xmodmap

setxkbmap -layout gb

xautolock -secure \
          -time     10 -locker   "slock" &
/usr/bin/dropbox &
feh --bg-scale ~/docs/pics/wallpaper &
exec i3

