Todd's Portable Config Stuff
============================

    It's Madness!
    It's Config!
    It's Config Madness!


This tree is intended to be checked-out to ~/etc

Start by copying:
  * etc/bash/dotbashrc.sh to ~/.bashrc
  * etc/emacs/dotemacs.el to ~/.emacs
Customize those files as necessary.

Share and Enjoy!
-- todd@toddjonker.net


Bash Customizations
===================

The common module loads a bunch of other stuff, then host-specific settings
from  ~/.bashrc-localhost  and then  $BASH_LIBRARY/`hostname`.sh

Be careful when sharing a home directory between hosts and/or platforms!



Subversion Customizations
=========================

The bin/svn-viewdiff causes `svn diff` to popup the MacOS FileMerge program for
visual diffing.  To enable it, set the diff-cmd option in ~/.subversion/config
to the full path to the script.



TODO
====

Test the bash keybindings (common.sh).  Do they work on Linux and Cygwin?

Be smart about non-interactive shells, and don't load all this stuff.

Can ~/.subversion/config be pulled into source control too?

Test newer findr on Linux and Cygwin.
  *  -X doesn't work on linux.

Test keychain on Linux.
