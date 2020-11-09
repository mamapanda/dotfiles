eval $(keychain --eval id_rsa)

# https://wiki.archlinux.org/index.php/Xinit#Autostart_X_at_login
if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
fi
