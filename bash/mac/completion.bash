ss_source_next

# https://docs.brew.sh/Shell-Completion#configuring-completions-in-bash
if type brew &>/dev/null
then
    HOMEBREW_PREFIX="$(brew --prefix)"
    if ! ss_source_if_present "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
    then
        for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*
        do
            ss_source_if_present "$COMPLETION"
        done
    fi
    unset HOMEBREW_PREFIX
fi
