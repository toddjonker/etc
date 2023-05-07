# Avoid warnings per https://github.com/koalaman/shellcheck/wiki/SC1090
# shellcheck source=/dev/null


if [[ ! -d "/System" ]]
then
    echo "ERROR! Not on a Mac, shouldn't be loading Mac module"
fi


# https://docs.brew.sh/Shell-Completion
if [[ -d /opt/homebrew/bin ]]; then        
    HOMEBREW_PREFIX=/opt/homebrew
else
    HOMEBREW_PREFIX=/usr/local
fi



# We want to load (at least) git-prompt above before we get to the prompt setup.

ss_source_next
# The path module has been loaded by this point.


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


# https://docs.brew.sh/Shell-Completion#configuring-completions-in-bash
if type brew &>/dev/null
then
    HOMEBREW_PREFIX="$(brew --prefix)"
    if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]
    then
	source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
    else
	for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*
	do
	    if [[ -r "$COMPLETION" ]]
	    then
		ss_log "Sourcing $COMPLETION"
		source "$COMPLETION"
	    fi
	done
    fi
fi
