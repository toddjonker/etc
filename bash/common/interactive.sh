# This is the main ShellSeed module for interactive shells.


if [[ -n $BASH_VERSION ]]
then
  shopt -s checkwinsize
  shopt -s no_empty_cmd_completion
  shopt -s nocaseglob     # Case-insensitive filename completion
  shopt -u sourcepath     # Keep source and . commands from searching PATH
fi

if [[ -n $ZSH_VERSION ]]
then
  setopt auto_cd      # If unknown command is a directory, cd into it.
  setopt correct      # Autocorrect misspelled commands
  setopt correct_all  # Autocorrect misspelled arguments
fi


export PAGER=less
export LESS="-FQRX"
export EDITOR=emacs     # TODO This shouldn't be here.

ss_load_modules batch git bindings completion prompt file
