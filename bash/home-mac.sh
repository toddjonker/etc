#echo "default PATH=$PATH"

declare -a baseBins=(
#    ~/bin
#    ~/dev/tools/maven-2.0.4/bin
)

for bin in "${baseBins[@]}"
do
    if [ -d "$bin" ]
    then
	PATH="$PATH:${bin}"
    fi
done

unset baseBins


USER_BASH_MODULES="mac bindings prompt file ssh seedling git"

## TEMPORARY
alias seedling-release=~/src/seedling/bin/release-build

cd ~

## EOF ##
