#!/bin/bash
# https://wiki.archlinux.org/index.php/Polybar#Running_with_WM

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do
    sleep 1;
done

# Launch Polybar, using default config location ~/.config/polybar/config
polybar main &
