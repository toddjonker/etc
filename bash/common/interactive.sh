# This is the main ShellSeed module for interactive shells.

export PAGER=less
export LESS="-FQRX"
export LESSCHARSET=utf-8
export EDITOR=emacs     # TODO This shouldn't be here.

ss_load_modules batch options bindings prompt completion termtitle \
                file direnv git java
