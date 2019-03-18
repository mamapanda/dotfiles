#! /usr/bin/env bash

stowdirs=("alacritty" "bash" "emacs" "lang" "nvim")

for stowdir in ${stowdirs[*]}; do
    stow $stowdir
done
