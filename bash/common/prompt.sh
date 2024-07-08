
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
    ss_load_modules git-prompt

    # This makes it easier to inject stuff into the prompt.
    PS1_PREFIX='\[\e[1;35m\][\W'
    PS1_SUFFIX=']\$\[\e[0m\] '
    PS1=$PS1_PREFIX${PS1_GIT_BITS}$PS1_SUFFIX
fi
