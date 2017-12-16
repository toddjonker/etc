if [[ ! -d "/System" ]]
then
    echo "ERROR! Not on a Mac, shouldn't be loading Mac module"
fi


ss_source_next


pathmunge "$USER_LIBRARY/bin/mac" after

    
alias oe="open -a Emacs"

alias ost="open -a SourceTree"
alias ost.="open -a SourceTree ."


# https://www.iterm2.com/documentation-shell-integration.html
if [[ $ITERM_SESSION_ID && -f ~/.iterm2_shell_integration.bash ]]
then
    source ~/.iterm2_shell_integration.bash
fi
