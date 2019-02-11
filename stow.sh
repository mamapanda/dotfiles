#! /usr/bin/env bash

stowdirs=("bash" "emacs" "r")

for stowdir in ${stowdirs[*]}; do
    stow $stowdir
done
