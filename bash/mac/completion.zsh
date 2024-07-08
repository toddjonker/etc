# https://docs.brew.sh/Shell-Completion#configuring-completions-in-zsh
if type brew &>/dev/null
then
    HOMEBREW_PREFIX="$(brew --prefix)"

    FPATH="${HOMEBREW_PREFIX}/share/zsh/site-functions:${FPATH}"

    # Homebrew doesn't include this in site-functions!
    GP="${HOMEBREW_PREFIX}/etc/bash_completion.d/git-prompt.sh"
    if [[ -r "$GP" ]]
    then
        ss_log "Sourcing $GP"
        source "$GP"
    else
        ss_log "No git-prompt.sh at $GP"
    fi

    unset GP HOMEBREW_PREFIX
fi

ss_source_next
