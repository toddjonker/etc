##  This file should be symlinked from ~/.bashrc
##  It is loaded by each Bash subshell.
##  See .bash_profile for code that is run only on first login.


USER_LIBRARY=${USER_LIBRARY:-~/etc}
BASH_LIBRARY=${USER_LIBRARY}/bash


if [ -e ~/bin ]
then
    PATH=$PATH:~/bin
fi
PATH=$PATH:$USER_LIBRARY/bin

 
if [ -n "$PS1" ]
then
    shopt -s checkwinsize
    shopt -s no_empty_cmd_completion
    shopt -s nocaseglob     # Case-insensitive filename completion
    shopt -u sourcepath     # Keep source and . commands from searching PATH
 
    export PAGER=less
    export LESS="-FQRX"
    export EDITOR=emacs
fi


##
##  Host-specific stuff is loaded from the first readable file among:
##
##    ~/.bashrc-localhost
##       -- Works well for things that don't need revision control
##    $BASH_LIBRARY/`hostname`.sh
##    $BASH_LIBRARY/local.sh
##       -- Tends to cause revision control problems, deprecate?

# If you're sharing home directories between different platforms, you probably
# should use HOSTNAME.sh instead of .bashrc-localhost

# The loaded file should define USER_BASH_MODULES to list the modules to load.
# But they will currently only be loaded for interactive shells.


if [ -r ~/.bashrc-localhost ]
then
    . ~/.bashrc-localhost
elif [ -r "$BASH_LIBRARY/`hostname`.sh" ]
then
    . "$BASH_LIBRARY/`hostname`.sh"
elif [ -r "$BASH_LIBRARY/local.sh" ]
then
    . "$BASH_LIBRARY/local.sh"
fi


#  TODO the interaction test needs to be pushed down into the modules.

if [ -n "$PS1" ]
then
    for lib in ${USER_BASH_MODULES:-bindings prompt file ssh}
    do
	if [ -r "$BASH_LIBRARY/${lib}-local.sh" ]
	then
	    . "$BASH_LIBRARY/${lib}-local.sh"
	else
	    . "$BASH_LIBRARY/${lib}.sh"
	fi
    done
fi
unset USER_BASH_MODULES


## EOF ##
