#!/bin/sh

stow --target=$HOME --ignore="sync.sh" .

[ ! -e "${HOME}/.config/rofi/themes" ] &&
    ln -s "${ROFI_THEME_DIR}" "${HOME}/.config/rofi/themes"
[ ! -e "${HOME}/.Xresources" ] &&
    ln -s "${XRESOURCES_CONFIG}" "${HOME}/.Xresources"
