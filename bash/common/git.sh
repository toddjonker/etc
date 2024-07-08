
: "${GIT_HOME:=/usr/local/git}"
if [ -d "$GIT_HOME/bin" ]
then
    pathmunge "$GIT_HOME/bin" before
fi
unset GIT_HOME

alias sm="git sm"
alias p="git pull --ff-only"
alias s="git status"
alias smp="git sm && git pull --ff-only"
