#! /usr/bin/env sh

export $(envsubst < ${HOME}/.env)
# export NOTMUCH_CONFIG="${HOME}/.config/notmuchrc"

[[ -f "${HOME}/.bashrc" ]] && source ~/.bashrc

# added by Nix installer
if [ -e /home/ratyaa/.nix-profile/etc/profile.d/nix.sh ]
then
    source /home/ratyaa/.nix-profile/etc/profile.d/nix.sh
fi

# if running bash
if [ -n "${BASH_VERSION}" ]
then
    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]
    then
	source "${HOME}/.bashrc"
    fi
fi
