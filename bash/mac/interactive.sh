# Avoid warnings per https://github.com/koalaman/shellcheck/wiki/SC1090
# shellcheck source=/dev/null


if [[ ! -d "/System" ]]
then
    echo "ERROR! Not on a Mac, shouldn't be loading Mac module"
fi


ss_source_next
# The path module has been loaded by this point.


pathmunge "$USER_LIBRARY/bin/mac" after

    
alias oe="open -a Emacs"

alias ost="open -a SourceTree"
alias ost.="open -a SourceTree ."


if [[ $ITERM_SESSION_ID ]]
then
    # https://www.iterm2.com/documentation-shell-integration.html
    [[ -n $BASH_VERSION ]] && ss_source_if_present ~/.iterm2_shell_integration.bash
    [[ -n $ZSH_VERSION  ]] && ss_source_if_present ~/.iterm2_shell_integration.zsh

    # https://iterm2.com/documentation-utilities.html
    test -d ~/.iterm2 && pathmunge ~/.iterm2 after
fi
