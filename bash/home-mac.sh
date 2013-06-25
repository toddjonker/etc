#echo "default PATH=$PATH"

declare -a baseBins=(
    /usr/local/bin
#    ~/bin
    ~/dev/tools/maven-2.0.4/bin
)

for bin in "${baseBins[@]}"
do
    if [ -d "$bin" ]
    then
	PATH="$PATH:${bin}"
    fi
done

unset baseBins

# MacOS 10.6 has svn in /usr/bin
PATH=/opt/subversion/bin:"$PATH"


USER_BASH_MODULES="bindings prompt file ssh mac seedling git svn"

## TEMPORARY
alias seedling-release=~/src/seedling/bin/release-build

cd ~

## EOF ##
