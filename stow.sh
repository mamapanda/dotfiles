#! /usr/bin/env bash

stowdirs=("bash" "emacs" "lang" "nvim")

for stowdir in ${stowdirs[*]}; do
    stow $stowdir
done
