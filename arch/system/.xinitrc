#!/bin/bash

export GDK_SCALE=1
export GDK_DPI_SCALE=0.75
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCREEN_SCALE_FACTORS=1.5

xrandr --dpi 192

[ -f ~/.bash_profile ] && . ~/.bash_profile
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

dwmblocks &
exec dwm
