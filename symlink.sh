#! /usr/bin/bash

files=(".emacs.d" ".bashrc" ".bash_profile")

for file in ${files[*]}; do
    fullpath="$(pwd -P)/$file"
    symlinkpath="$HOME/$file"

    if [ -e "$symlinkpath" ]; then
        echo "Warning: $symlinkpath already exists. No action taken."
    else
        ln -s "$fullpath" "$symlinkpath"
        echo "Created $symlinkpath to point to $fullpath."
    fi
done
