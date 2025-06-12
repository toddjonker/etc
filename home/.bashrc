# -*- shell-script -*-

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


SHELLSEED_LOG=~/shellseed.log
[[ -f $SHELLSEED_LOG ]] && rm "$SHELLSEED_LOG"

. ~/etc/bash/shellseed.sh

shellseed_use_libraries cdd
shellseed_init
