
# This causes p4 to search for this file, where you should define things for
# each client.  This avoids having trouble switching between depots in the
# same shell.  The file should look like:
#
#  P4PORT=...
#  P4CLIENT=...

export P4CONFIG=.p4config

export P4EDITOR='emacs -nw'


alias p4-missing='find . -type f | xargs p4 files 2>&1 | perl -ne "/(\\S+) - no such file/ && print qq(\$1\\n)" | xargs p4 opened 2>&1 | grep "not opened" | cut -d" " -f1'
