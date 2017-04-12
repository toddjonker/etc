if [ -d "/System" ]
then
    # Mac OS X

    # TODO This may be unnecessary, I think its on the default PATH now.
    if [ -d /usr/local/bin ]
    then
	PATH=$PATH:/usr/local/bin
    fi

    PATH=$PATH:$USER_LIBRARY/bin/mac

    # Homebrew installs Git completions here:
    GIT_COMPLETION=/usr/local/etc/bash_completion.d
    
    alias oe="open -a Emacs"

    alias ost="open -a SourceTree"
    alias ost.="open -a SourceTree ."
fi
