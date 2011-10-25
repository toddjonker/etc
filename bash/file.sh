###   tjonker's aliases file                                 -*-shell-script-*-
###
###   All machines should source this file to set up standard aliases.
###
###   This was last tested using GNU bash 2.05.0


# Make some commands a bit more safe
alias rm='rm -i'
alias rmyes='/bin/rm -r'
alias mv='mv -i'
alias cp='cp -p'

alias sudoshell='sudo -s -H -u'


if [ "$TERM" = "emacs" ]
then
    alias ls='ls -F'
    alias lsa='ls -AF'
    alias lal='ls -AFl'
else
    alias ls='ls -FG'
    alias lsa='ls -AFG'
    alias lal='ls -AFGl'
fi

alias ..='cd ..; echo $PWD'

alias m=less



# File links, etc
#alias lock      chmod go-rwx
#alias unlock	'chmod u+w \!*; chmod go+rx \!*'
#alias ln	'echo Use link or /bin/ln -s;echo \!*>/dev/null'
#alias link	/bin/ln -s
#alias mlink	'/bin/mv \!:1 \!:2; link \!:2 \!:1'  # move and link

# look for text in elisp source files

# print path in a single column
#alias ppath echo \$path \| "awk '{for(i=1; i<=NF; i++) printf" '"%3d: %s\n", i, $i}'"'"


findr()
{
    # New version via mtoulou@amazon.com
    # Using find -print0 and xargs -0 allows this to work with files with
    # spaces (and even newlines) in their names.
    # It also greps 1000 files at a time, so it's a fair bit faster than
    # using find -exec.

    # Note that Linux doesn't support the options that MacOS does (eg -X)
    find . -type f \( -not -path "*/.svn/*" \) -print0 | xargs -0 -n 1000 grep "$*"
}

find-exe()
{
    find . \( -not -type d \) \( -not -path \*/.svn/\* \) -perm +111
}


alias clean='rm *BAK *~ *% .*BAK .*~ .*% #*# core*'

cleansrc()
{
    find . -type f \( -name "*~" -o -name "\.DS_Store" -o -name "#*#" \) \
	-exec /bin/rm {} \;
}

##  EOF  ##
