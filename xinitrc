xsetroot -cursor_name left_ptr
setxkbmap -layout gb
feh --bg-scale /home/nuchs/docs/wallpaper/tardis.jpg

urxvtd -f -o -q
urxvt&
urxvt&
chromium&

exec xmonad
