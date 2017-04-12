: ${GIT_HOME:=/usr/local/git}
: ${GIT_CONTRIB:=$GIT_HOME/contrib}
: ${GIT_COMPLETION:=$GIT_CONTRIB/completion}


# TODO this is wrong, the dir may already be in $PATH

if [ -d "$GIT_HOME/bin" ]
then
    PATH="$GIT_HOME/bin":$PATH
fi


# These files contain good documentation on their use.
# I last read through it at git 1.8
# Git 1.8 splits the prompt stuff out.


if [ -r "$GIT_COMPLETION/git-completion.bash" ]
then
    source "$GIT_COMPLETION/git-completion.bash"
fi

if [ -r "$GIT_COMPLETION/git-prompt.sh" ]
then
    source "$GIT_COMPLETION/git-prompt.sh"
fi


# Did we find prompt completion code?
if (declare -F __git_ps1 >& /dev/null)
then
    GIT_PS1_SHOWDIRTYSTATE=1
#   GIT_PS1_SHOWSTASHSTATE=1
#   GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWUPSTREAM=auto

    PS1=$PS1_PREFIX'$(__git_ps1 " (%s)")'$PS1_SUFFIX
fi

unset GIT_HOME GIT_CONTRIB GIT_COMPLETION
