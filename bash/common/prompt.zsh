# https://zsh.sourceforge.io/Doc/Release/Prompt-Expansion.html

PROMPT='%F{magenta}%3~ %#%f '


# Add VCS info to the right prompt

# Git provides more flexible mechanisms than ZSH, so let's try that.
ss_load_modules git-prompt

if [[ -n "$PS1_GIT_BITS" ]]
then
    # This is disabled by default since it messes with
    # how the Bash prompt is colored.
    GIT_PS1_SHOWCOLORHINTS=1
else
    # Fall-back to built-in ZSH functionality
    # https://zsh.sourceforge.io/Doc/Release/User-Contributions.html#Version-Control-Information

    autoload -Uz vcs_info
    precmd_vcs_info() { vcs_info }
    precmd_functions+=( precmd_vcs_info )

    zstyle ':vcs_info:*' enable git
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' unstagedstr '*'                           # Output for %u
    zstyle ':vcs_info:*' stagedstr   '+'                           # Output for %c
    zstyle ':vcs_info:git:*' formats '%F{cyan}(%b%u%c)%%f'

    PS1_GIT_BITS='${vcs_info_msg_0_}'
fi

setopt prompt_subst
RPROMPT=$PS1_GIT_BITS
