# The sequence  %{\033[#;#m%}   sets the ANSI colour or attribute specified 
# by # (or several attributes can be specified by #;#;#...).
# Possible values for # are:
#
# Attributes:
#    0 reset, 1 bold, 4 underline 5 flashing 7 inverse
# Text:
#    30 black, 31 red,     32 green, 33 yellow
#    34 blue,  35 magenta, 36 cyan,  37 white
#    90-97 are lighter variants of the above colours
# Background:
#    40-47, colours as for text
#    100-107 are lighter variants


# Derek Upham writes:
# Emacs shell mode runs with $TERM set to 'dumb', so you should distinguish
# between that and other color-supporting terminal types when setting PS1:
#
#  case $TERM in
#      xterm* | rxvt | vt220 | screen | eterm-color)
#          PS1=... # colorful prompt
#          ;;
#      *)
#          PS1=... # dumb prompt
#          ;;
#  esac
#
# 'eterm-color' is the terminal type of the 'term' subshell.  'term' is a full
# terminal emulator handling VT100 escape codes, while 'shell' doesn't support
# escape codes.


if [ "$TERM" = "dumb" ]
then
    set -o emacs
    PS1="[\W]\$ ";
elif [ "$TERM" = "emacs" ]
then
    PS1="[\W]\$ ";
else
    echo hi ${USER}
#    PS1="[\w]\$ "

    set-title()
    {
	TERMINAL_WINDOW_TITLE=$*
    }

    # This is convoluted since hostname -s isn't universal.
    set-title "${USER}@`hostname | cut -d . -f 1`"
    
    # This magic sequence is what actually changes the terminal title.
    # We use PROMPT_COMMAND to set it every time the prompt is printed.
    if [ "$TERM" != "linux" ] ; then
        PROMPT_COMMAND='echo -ne "\033]0;${TERMINAL_WINDOW_TITLE}\007"'
    fi

#   PS1="\[\033[1;35m\][\u@\h:\W]\$\[\033[0m\] "
    PS1="\[\033[1;35m\][\W]\$\[\033[0m\] "
fi
