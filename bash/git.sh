: ${GIT_HOME:=/usr/local/git}
: ${GIT_CONTRIB:=$GIT_HOME/contrib}


if [ -d /usr/local/git/bin ]
then
    PATH=$PATH:/usr/local/git/bin
fi

if [ -r "$GIT_CONTRIB/completion/git-completion.bash" ]
then
    # Read this file for documentation on this tool.
    # I last read through it at git 1.7.7

    source "$GIT_CONTRIB/completion/git-completion.bash"

    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWUPSTREAM=auto

    PS1=$PS1_PREFIX'$(__git_ps1 " (%s)")'$PS1_SUFFIX
fi

unset GIT_HOME GIT_CONTRIB
