if [ -d "/System" ]
then
    # Mac OS X
    export JAVA_HOME=/Library/Java/Home

    # Note that localhost doesn't work below, at least with OSXvnc 1.5
    # -T to disable pseudo-tty allocation
    # -N so you do not get a shell and cannot execute commands
    # See http://www.trekweb.com/~jasonb/articles/vnc_ssh.shtml
    alias vnc-tunnel-bigweld='ssh -L 5900:127.0.0.1:5900 -N -T bigweld.local'

    if [ -d /usr/local/bin ]
    then
	PATH=$PATH:/usr/local/bin
    fi

    PATH=$PATH:$USER_LIBRARY/bin/mac
fi
