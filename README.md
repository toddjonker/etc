Todd's Portable Config Stuff
============================

    It's Madness!
    It's Config!
    It's Config Madness!


This tree is intended to be checked-out to ~/etc

Start by symlinking:
  * ~/.bashrc to etc/bash/dotbashrc.sh
  * ~/.emacs  to etc/emacs/dotemacs.el
  * ~/.gitconfig to etc/git/config

Share and Enjoy!
-- todd@toddjonker.net


Bash Customizations
===================

The common module loads a bunch of other stuff, then host-specific settings
from  ~/.bashrc-localhost  and then  $BASH_LIBRARY/`hostname`.sh

Be careful when sharing a home directory between hosts and/or platforms!


TODO
====

git.sh - Generalize locating the git-completion script.

prompt.sh - some cruft for Linux that I don't understand.

bindings.sh - Test and look for new ones. Do they work on Linux and Cygwin?

Test newer findr on Linux and Cygwin.
  *  -X doesn't work on linux.
