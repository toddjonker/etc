##  This file should be sourced from ~/.bashrc


## TODO
##
##  * Add notion of libraries, so one can override things.
##    Then a host will select a set of libraries to use.


USER_LIBRARY=${USER_LIBRARY:-~/etc}
[[ -r "$USER_LIBRARY" ]] || echo Bad USER_LIBRARY=$USER_LIBRARY

BASH_LIBRARY=${USER_LIBRARY}/bash


#------------------------------------------------------------------------------
# Cleanup mechanism

function ss_unset_later()
{
    # This is a space-separated list of shell items to be unset
    # at the end of the setup process.
    _SS_UNSET_LATER="$_SS_UNSET_LATER ""$@"
}

ss_unset_later ss_unset_later _SS_UNSET_LATER


#------------------------------------------------------------------------------
# Logging

function ss_log()
{
    if [[ -n "$SHELLSEED_LOG" ]]
    then
	echo "$@" >> "$SHELLSEED_LOG"
    fi
}

ss_unset_later ss_log SHELLSEED_LOG

ss_log "---------------------"
ss_log $(date)


#------------------------------------------------------------------------------
# Module loading

if [ -n "$PS1" ]
then
    SHELLSEED_MODULES=${SHELLSEED_MODULES:-${SHELLSEED_INTERACTIVE_MODULES:-interactive}}
else
    SHELLSEED_MODULES=${SHELLSEED_MODULES:-batch}
fi

ss_unset_later SHELLSEED_MODULES SHELLSEED_INTERACTIVE_MODULES


function ss_load_modules()
{
    # _SS_LOADED_MODULES is colon-separated list of modules.
    # To break (but not detect) cycles, modules are added before being loaded.

    local modname

    for modname in "$@"
    do
	case ":${_SS_LOADED_MODULES}:" in
	    *:"$modname":*)
		ss_log "Already loaded $modname"
	        ;;
	    *)
		_SS_LOADED_MODULES=$_SS_LOADED_MODULES:$modname

		ss_log "Loading $modname"

		# TODO Define SHELLSEED_LOADING_MODULE
		#  being careful to handle the recursive case

		. "$BASH_LIBRARY/$modname.sh"
	esac
    done
}

ss_unset_later ss_load_modules _SS_LOADED_MODULES


ss_load_modules ${SHELLSEED_MODULES}

unset $_SS_UNSET_LATER
