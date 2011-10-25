##  .bashrc
##
##  This file is loaded by each Bash subshell.
##  See .bash_profile for code that is run only on first login.


# Don't quote this, or the ~ won't expand:
USER_LIBRARY=~/etc
BASH_LIBRARY="${USER_LIBRARY}/bash"

. "$BASH_LIBRARY/common.sh"

# Host-specific stuff is loaded from:
#    ~/.bashrc-localhost
#    $BASH_LIBRARY/`hostname`.sh


# Remove things you don't need:

. "$BASH_LIBRARY/mac.sh"
. "$BASH_LIBRARY/cygwin.sh"
#. "$BASH_LIBRARY/amazon.sh"
. "$BASH_LIBRARY/seedling.sh"


# More host-specific stuff can go here.

#export JAVA_HOME=/cygdrive/c/java/j2sdk1.4.2_05


cd ~

## EOF ##
