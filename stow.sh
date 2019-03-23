#! /usr/bin/env bash

stowdirs=("alacritty" "bash" "emacs" "i3" "lang" "nvim" "x")

for stowdir in ${stowdirs[*]}; do
    stow $stowdir
done
