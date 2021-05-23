#!/bin/bash
export BROWSER=firefox
export DE=i3

hsetroot -full ~/.wallpaper.jpg &

## DPI: 100%: 96, 125%: 120, 150%: 144, 175%: 168, 200%: 192
## DPI, backlight, Xresources
xrdb -merge ~/.Xresources
xrandr --dpi 120
xbacklight -set 10
xset -b
#xinput set-prop 12 278 1
#xinput set-prop 12 286 1


## map the caps lock key to Ctrl key
setxkbmap -option 'ctrl:nocaps'

## start the notification daemon
/usr/lib/notify-osd/notify-osd &

## start the nm-applet
nm-applet &
volumeicon &
conky &
blueman-applet &

## start parcellite
parcellite &

## fix touchpad
xinput set-prop "DLL06FD:01 04F3:300F Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "DLL06FD:01 04F3:300F Touchpad" "libinput Natural Scrolling Enabled" 1
#xinput set-prop "DLL06FD:01 04F3:300F Touchpad" "" 1

## start redshift-gtk
redshift-gtk -l 21:77 &

onboard &

## Run the window manager session
exec i3
#exec xterm
#exec fvwm
