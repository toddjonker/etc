# Git prompt functions are loaded by bash_completion, so
# make sure we run that first.
ss_load_modules completion

# https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh

if type __git_ps1 >& /dev/null
then
    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWUPSTREAM=auto
#   GIT_PS1_SHOWCOLORHINTS=1    # Screws up Bash prompt

    # shellcheck disable=SC2016
    PS1_GIT_BITS='$(__git_ps1 " (%s)")'

    ss_unset_later PS1_GIT_BITS
fi
