# This is the main ShellSeed module for interactive ZSH shells.


setopt auto_cd      # If unknown command is a directory, cd into it.
setopt correct      # Autocorrect misspelled commands
setopt correct_all  # Autocorrect misspelled arguments


export PAGER=less
export LESS="-FQRX"
export LESSCHARSET=utf-8
export EDITOR=emacs     # TODO This shouldn't be here.

ss_load_modules batch git bindings completion prompt file
