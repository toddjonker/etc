###
###  Development helpers
###

alias gcr="./gradlew clean release"
alias gr="./gradlew release"
alias gv="./gradlew --console=verbose"
alias gw="./gradlew"


jfindr()
{
    find . \( -name "*.java" -o -name "*.jsp" -o -name "*.jhtml" -o -name "*.properties" -o -name "*.xml" \) -exec egrep "$*" {} /dev/null \;
}


checkjars()
{
    for i in *.jar ; do echo "  $i" ; jarsigner -verify "$i" ; done
}



# Search-and-replace within java files

jsubst()
{
    for i in $(find . -type f \( -name "*.java" -o -name "*.properties" \) -print0 | xargs -0)
    do 
        sed -e "s/$1/$2/g" < "$i" > "$i.new"
        mv -f "$i.new" "$i"
    done
}
