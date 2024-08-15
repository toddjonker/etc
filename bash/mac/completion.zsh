# https://docs.brew.sh/Shell-Completion#configuring-completions-in-zsh
if type brew &>/dev/null
then
    HOMEBREW_PREFIX="$(brew --prefix)"

   fpath=( ${HOMEBREW_PREFIX}/share/zsh/site-functions $fpath )

    # Homebrew doesn't include this in site-functions!
    ss_source_if_present "${HOMEBREW_PREFIX}/etc/bash_completion.d/git-prompt.sh"

    unset HOMEBREW_PREFIX
fi

ss_source_next
