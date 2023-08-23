#!/bin/sh

sxhkd &
xrdb -merge $HOME/.Xresources

xset r rate 300 150
fcitx5 -d &

pipewire &
pipewire-pulse &
wireplumber &

emacs --daemon &

eval "$(ssh-agent -s)"

light -S 50
redshift -P -O 4500
picom -b
feh --bg-scale $HOME/pics/wallpapers/goofyplants.png 

exec bspwm
