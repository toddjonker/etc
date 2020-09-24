if [[ ! -d "/System" ]]
then
    echo "ERROR! Not on a Mac, shouldn't be loading Mac module"
fi


ss_source_next


pathmunge "$USER_LIBRARY/bin/mac" after

    
alias oe="open -a Emacs"

alias ost="open -a SourceTree"
alias ost.="open -a SourceTree ."


if [[ $ITERM_SESSION_ID ]]
then
    # https://www.iterm2.com/documentation-shell-integration.html
    test -f ~/.iterm2_shell_integration.bash && source ~/.iterm2_shell_integration.bash

    # https://iterm2.com/documentation-utilities.html
    test -d ~/.iterm2 && pathmunge ~/.iterm2 after
fi

source /usr/local/etc/bash_completion.d/brew
