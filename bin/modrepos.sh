#!/bin/bash


SRC=src

#rm -rf $SRC/bonsai

mv $SRC/bonsai/main/toddstuff $SRC/bonsai/main/consciouscode
mv $SRC/bonsai/test/toddstuff $SRC/bonsai/test/consciouscode


mv $SRC/jelly/main/toddstuff $SRC/jelly/main/consciouscode
mv $SRC/jelly/test/toddstuff $SRC/jelly/test/consciouscode

mv $SRC/launcher/main/toddstuff $SRC/launcher/main/consciouscode

mv $SRC/runtime/main/toddstuff $SRC/runtime/main/consciouscode
mv $SRC/runtime/test/toddstuff $SRC/runtime/test/consciouscode

#mv $SRC/sample1 $SRC/ticker


cat > update.pl <<EOF
#!/usr/bin/perl

\$lineNumber=0;

foreach (<STDIN>)
{
    if (\$lineNumber < 3)
    {
	s/(C) 2004 Todd/(C) 2004-2005 Todd/;
	s/-200[1234] Todd/-2005 Todd/;
	s/Todd V. Jonker./Conscious Code Ltd./;
    }

    s/toddstuff/consciouscode/g;

    print \$_;
    \$lineNumber++; 
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
