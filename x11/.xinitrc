#!/bin/bash

## set wallpaper
hsetroot -full ~/.wallpaper.jpg &

## merge Xresources
xrdb -merge ~/.Xresources

# scale DPI for hi-res screens
## DPI: 100%: 96, 125%: 120, 150%: 144, 175%: 168, 200%: 192
xrandr --dpi 120

# Set backlight to 10%
xbacklight -set 10

# disable bell
xset -b

## map the caps lock key to Ctrl key
setxkbmap -option 'ctrl:nocaps'

## start the notification daemon
/usr/lib/notify-osd/notify-osd &

## start the network manager applet
nm-applet &

## volume control applet
volumeicon &

## desktop monitor
conky &

## bluetooth applet
blueman-applet &

## start clipboard manager
parcellite &

## fix touchpad tapping and scroll direction
xinput set-prop "DLL06FD:01 04F3:300F Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "DLL06FD:01 04F3:300F Touchpad" "libinput Natural Scrolling Enabled" 1
#xinput set-prop "DLL06FD:01 04F3:300F Touchpad" "" 1

## start redshifting for Nagpur
redshift-gtk -l 21:77 &

## onscreen keyboard for touch mode
onboard &

## Run the window manager session
exec i3
