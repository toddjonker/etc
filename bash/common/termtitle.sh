# Functions to set the terminal window title

set-title()
{
    TERMINAL_WINDOW_TITLE=$*
    # This by itself doesn't have much effect; it's used by
    # PROMPT_COMMAND to change the title when the prompt is printed.
}

st.()
{
    set-title "$(basename "$(pwd)")"
}

alias st=set-title

# This is convoluted since hostname -s isn't universal.
TVJ_DISPLAY_HOSTNAME=${TVJ_DISPLAY_HOSTNAME:-$(hostname | cut -d . -f 1)}

reset-title()
{
    set-title "${USER}@${TVJ_DISPLAY_HOSTNAME:-$(hostname)}"
}

reset-title

update-terminal-title()
{
    # This magic sequence is what changes the terminal title.
    echo -ne "\033]0;${TERMINAL_WINDOW_TITLE}\007"
}


# Use prompt commands to update the title.
# This ensures the title is updated when return from subshells.
if [[ -n $ZSH_VERSION ]]
then
    precmd_functions+=( update-terminal-title )
elif [[ -n $BASH_VERSION ]]
then
    # Be careful to preserve ITerm2 integration command.
    PROMPT_COMMAND='update-terminal-title; '"$PROMPT_COMMAND"
fi
