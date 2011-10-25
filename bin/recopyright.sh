#!/bin/bash


SRC=src

cat > update.pl <<EOF
#!/usr/bin/perl

foreach (<STDIN>)
{
    s/Conscious Code Ltd./Todd V. Jonker./;
    print \$_;
}
EOF
chmod a+x update.pl

for i in `find src doc bin build -type f \( -name "*.java" -o -name "*.properties" -o -name "*.properties.template" -o -name "*.xml" -o -name "*.html" -o -name "*.jelly" -o -name "*.TXT" \) | xargs`
do 
  cat $i | ./update.pl > $i.new
  mv -f $i.new $i 
done

#rm update.pl


# From Ittigson
# for i in `find -type f -name "*.java" | xargs`; do cat $i | perl -e '$c=0; foreach (<STDIN>) { s/Inpath Solutions, LLC/Todd V. Jonker/ if $c < 3;  print $_; $c++; }' > $i.new; mv $i.new $i; done
