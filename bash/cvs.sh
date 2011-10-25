
export CVS_RSH=ssh

cvsmod()
{
    if [ ! -d "CVS" ]
    then
	echo "Not in a CVS-managed directory."
    else
        cvs -nq update | grep "^[M?ARUC] "
    fi
}
