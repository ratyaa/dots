#!/bin/sh

[ -f $HOME/.xprofile ] && source $HOME/.xprofile

sxhkd &
xrdb -merge $HOME/.Xresources

xset r rate 300 150
fcitx5 -d &

# Device-specific actions
case $(hostname) in
    "ratyaaxd")
	xrandr --output HDMI-1-0 --auto --output eDP-1 --off
	
	systemctl --user --now enable wireplumber.service
	systemctl --user --now enable pipewire
	systemctl --user --now enable pipewire-pulse
	;;
    "ratyaaha")
	pipewire &
	pipewire-pulse &
	wireplumber &
	;;
esac

emacs --daemon &

sxhkd &
dunst &
xrdb -merge $HOME/.Xresources
xset r rate 300 150
fcitx5 -d &	

eval "$(ssh-agent -s)"

light -S 50
redshift -P -O 4500
picom -b

feh --bg-scale $HOME/.config/wallpaper 

exec bspwm
