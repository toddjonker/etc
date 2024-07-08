ss_source_next

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
    unset HOMEBREW_PREFIX
fi
