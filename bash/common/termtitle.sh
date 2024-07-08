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


# This magic sequence is what actually changes the terminal title.
# We use PROMPT_COMMAND to set it every time the prompt is printed.
if [ "$TERM" != "linux" ]
then
    # Be careful to preserve ITerm2 integration command.
    PROMPT_COMMAND='echo -ne "\033]0;${TERMINAL_WINDOW_TITLE}\007"; '"$PROMPT_COMMAND";
fi
