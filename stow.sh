#!/usr/bin/env bash

stowdirs=("compton" "emacs" "fish" "i3" "kitty" "lang" "oomox" "polybar" "x")

for stowdir in ${stowdirs[*]}; do
    stow $stowdir
done
