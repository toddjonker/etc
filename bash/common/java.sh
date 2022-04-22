###
###  Development helpers
###

jfindr()
{
    find . \( -name "*.java" -o -name "*.jsp" -o -name "*.jhtml" -o -name "*.properties" -o -name "*.xml" \) -exec egrep "$*" {} /dev/null \;
}


checkjars()
{
    for i in *.jar ; do echo "  $i" ; jarsigner -verify "$i" ; done
}



# Here's a sample bash command for search-and-replace within java files
# for i in `find . -type f -name "*.java" | xargs`; do cat $i | perl -e '$c=0; foreach (<STDIN>) { s/Inpath Solutions, LLC/Todd V. Jonker/ if $c < 3;  print $_; $c++; }' > $i.new; mv -f $i.new $i; done

jsubst()
{
    for i in $(find . -type f \( -name "*.java" -o -name "*.properties" \) -print0 | xargs -0)
    do 
      sed -e "s/$1/$2/g" < "$i" > "$i.new"
      mv -f "$i.new" "$i"
    done
}
