# This is the main ShellSeed module for interactive Bash shells.

shopt -s checkwinsize
shopt -s no_empty_cmd_completion
shopt -s nocaseglob     # Case-insensitive filename completion
shopt -u sourcepath     # Keep source and . commands from searching PATH


export PAGER=less
export LESS="-FQRX"
export LESSCHARSET=utf-8
export EDITOR=emacs     # TODO This shouldn't be here.

ss_load_modules batch git bindings completion prompt file
