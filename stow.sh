#! /usr/bin/env bash

stowdirs=("compton" "emacs" "fish" "i3" "kitty" "lang" "nvim" "rofi" "oomox" "x")

for stowdir in ${stowdirs[*]}; do
    stow $stowdir
done
