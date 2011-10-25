if [ -e ~/bin ]
then
    PATH=$PATH:~/bin
fi
PATH=$PATH:$USER_LIBRARY/bin


shopt -u sourcepath     # Keep source and . commands from searching the PATH.
shopt -s nocaseglob	# Case-insensitive filename completion


bind '"\e[1~":beginning-of-line'          # home key
bind '"\e[3~":delete-char'                # del key
bind '"\e[4~":end-of-line'                # end key
bind '"\e[5~":history-search-backward'    # pgup key
bind '"\e[6~":history-search-forward'     # pgdn key 



. "$BASH_LIBRARY/file.sh"
. "$BASH_LIBRARY/prompt.sh"
. "$BASH_LIBRARY/java.sh"
. "$BASH_LIBRARY/cvs.sh"
. "$BASH_LIBRARY/svn.sh"
. "$BASH_LIBRARY/p4.sh"

export PAGER=less
export LESS="-FQX"
export EDITOR=emacs


# Start the SSH keychain. The first new terminal will request the passphrase.
$USER_LIBRARY/bin/keychain --quiet ~/.ssh/id_dsa
. ~/.ssh-agent-${HOSTNAME}


##
##  Machine-specific stuff
##

# If you're sharing home directories between different platforms, you probably
# should use HOSTNAME.sh instead of .bashrc-localhost

if [ -r ~/.bashrc-localhost ]
then
    . ~/.bashrc-localhost
fi

if [ -r "$BASH_LIBRARY/`hostname`.sh" ]
then
    . "$BASH_LIBRARY/`hostname`.sh"
fi
