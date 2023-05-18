: "${GIT_HOME:=/usr/local/git}"
: "${GIT_CONTRIB:=$GIT_HOME/contrib}"
: "${GIT_COMPLETION:=$GIT_CONTRIB/completion}"

if [ -d "$GIT_HOME/bin" ]
then
    pathmunge "$GIT_HOME/bin" before
fi


# These files contain good documentation on their use.
# I last read through it at git 1.8
# Git 1.8 splits the prompt stuff out.


if [ -r "$GIT_COMPLETION/git-completion.bash" ]
then
    ss_log Sourcing "$GIT_COMPLETION/git-completion.bash"
    source "$GIT_COMPLETION/git-completion.bash"
fi

if [ -r "$GIT_COMPLETION/git-prompt.sh" ]
then
    ss_log Sourcing "$GIT_COMPLETION/git-prompt.bash"
    source "$GIT_COMPLETION/git-prompt.sh"
fi

ss_load_modules completion

# Did we find prompt completion code?
if (declare -F __git_ps1 >& /dev/null)
then
    GIT_PS1_SHOWDIRTYSTATE=1
#   GIT_PS1_SHOWSTASHSTATE=1
#   GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWUPSTREAM=auto

    # shellcheck disable=SC2016
    PS1_GIT_BITS='$(__git_ps1 " (%s)")'

    ss_unset_later PS1_GIT_BITS
fi

unset GIT_HOME GIT_CONTRIB GIT_COMPLETION

alias sm="git sm"
alias p="git pull --ff-only"
alias s="git status"
alias smp="git sm && git pull --ff-only"
