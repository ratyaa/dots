#
# ~/.bashrc
#

. $HOME/.bash_aliases

export GPG_TTY=$(tty)

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# alias ls='ls --color=auto'
# alias grep='grep --color=auto'

C_CYAN="\[$(tput setaf 14)\]"
C_WHITE="\[$(tput setaf 7)\]"
C_BOLD="\[$(tput bold)\]"
C_RESET="\[$(tput sgr0)\]"

PS1="${C_BOLD}${C_CYAN}[\u@\h ${C_WHITE}\W${C_CYAN}]\$ ${C_RESET} "

# Automatically added by the Guix install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi

#
# /etc/bash/bashrc
#


