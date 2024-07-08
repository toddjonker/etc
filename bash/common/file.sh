# -*-shell-script-*-

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


# print path in a single column
#alias ppath echo \$path \| "awk '{for(i=1; i<=NF; i++) printf" '"%3d: %s\n", i, $i}'"'"


findr()
{
    local o OPTARG OPTIND OPTERR
    local from=

    # FreeBSD/macOS supports -f and requires a path.
    # Linux doesn't support -f and uses a default path of .
    # This adaptation does both.
    while getopts ":f:" o; do
        case "$o" in
            f) from="$from $OPTARG";;
            *) ;;                      # Other options are passed through to grep
        esac
    done
    shift $((OPTIND-1))

    # New version via mtoulou@amazon.com
    # Using find -print0 and xargs -0 allows this to work with files with
    # spaces (and even newlines) in their names.
    # It also greps 1000 files at a time, so it's a fair bit faster than
    # using find -exec.

    # -L tells find to traverse symlinks.
    # $from is intentionally unquoted to allow multiple -f paths.
    find -L \
        "${from:-.}" \
        -type f \
        \( -not -path "*/.git/*" \) \
        \( -not -path "*/.metadata/*" \) \
        \( -not -path "*/.svn/*" \) \
        \( -not -path "*/eclipse-bin/*" \) \
        -print0 | xargs -0 -n 1000 grep "$*"
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


webhere()
{
    port=3000
    echo "Starting server:  http://$(hostname):$port/"
    ruby -run -ehttpd . -p $port
}


##  EOF  ##
