if [ -d "/System" ]
then
    # Mac OS X

    # TODO This may be unnecessary, I think its on the default PATH now.
    if [ -d /usr/local/bin ]
    then
	PATH=$PATH:/usr/local/bin
    fi

    PATH=$PATH:$USER_LIBRARY/bin/mac


    alias ost="open -a SourceTree"
    alias ost.="open -a SourceTree ."
fi
