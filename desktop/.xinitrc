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
        /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
        # ~/.dropbox-dist/dropboxd
        fcitx -d -r

        amixer -q set Master mute

        compton -b
        nitrogen --restore

        clipit &
        xfce4-power-manager
        xautolock -time 10 -locker blurlock &

        setxkbmap -option 'ctrl:swapcaps'
        xcape -e 'Control_L=Escape'
        sxhkd &
        ;;
esac

exec $session