#!/bin/bash

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xrdb -merge ~/.Xresources

session=${1:-bspwm}

case $session in
    bspwm)
        lxpolkit &
        fcitx -d -r

        amixer -q set Master mute

        picom -b
        nitrogen --restore
        redshift &

        clipit &
        # xfce4-power-manager

        xautolock -time 10 -corners 00-- -locker "betterlockscreen -l" &
        xsetroot -cursor_name left_ptr &

        setxkbmap -option 'ctrl:swapcaps'
        xcape -e 'Control_L=Escape'
        sxhkd &
        ;;
esac

exec $session
