#! /usr/bin/env bash

stowdirs=("alacritty" "bash" "emacs" "i3" "kitty" "lang" "nvim" "oomox" "x")

for stowdir in ${stowdirs[*]}; do
    stow $stowdir
done
