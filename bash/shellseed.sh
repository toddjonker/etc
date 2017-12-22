###   Copyright (C) 2017 Todd V. Jonker.  All Rights Reserved.
###
###   Licensed under the Apache License, Version 2.0 (the "License");
###   you may not use this file except in compliance with the License.
###   You may obtain a copy of the License at
###
###       http://www.apache.org/licenses/LICENSE-2.0
###
###   Unless required by applicable law or agreed to in writing, software
###   distributed under the License is distributed on an "AS IS" BASIS,
###   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
###   See the License for the specific language governing permissions and
###   limitations under the License.


USER_LIBRARY=${USER_LIBRARY:-~/etc}
[[ -r "$USER_LIBRARY" ]] || echo Bad USER_LIBRARY=$USER_LIBRARY


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
	echo "$_SS_LOG_PREFIX$@" >> "$SHELLSEED_LOG"
    fi
}

ss_unset_later ss_log SHELLSEED_LOG _SS_LOG_PREFIX

ss_log "---------------------"
ss_log $(date)


#------------------------------------------------------------------------------
# Module loading


_SS_LIBRARY_ROOT=${USER_LIBRARY}/bash

declare -a _SS_LIBRARIES

function shellseed_use_libraries()
{
    local libname=$1; shift

    if [[ $# -ne 0 ]]
    then
        shellseed_use_libraries "$@"
    fi

    case " ${_SS_LIBRARIES[@]} " in
	*" $libname "*)
	    ss_log "Already registered $libname"
	    ;;
	*)
	    ss_log "Registering $libname"

            local libdir=$_SS_LIBRARY_ROOT/$libname
            if [[ -d $libdir ]]
            then
                local libfile=$libdir/use-libraries
                if [[ -f $libfile ]]
                then
                    shellseed_use_libraries $(cat "$libfile")
                fi
                
	        _SS_LIBRARIES=($libname ${_SS_LIBRARIES[@]})
            else
                ss_log "ERROR: No library named $libname in $_SS_LIBRARY_ROOT"
            fi
            ;;
    esac
}

ss_unset_later shellseed_use_libraries _SS_LIBRARIES _SS_LIBRARY_ROOT


function ss_source_next()
{
    # This uses global variables to make it easy on the user.

    while (( $_SS_CURRENT_LIBRARY < ${#_SS_LIBRARIES[*]} ))
    do
        local libname=${_SS_LIBRARIES[$_SS_CURRENT_LIBRARY]}

        _SS_CURRENT_LIBRARY=$(( $_SS_CURRENT_LIBRARY + 1 ))            

        ss_log "Looking for $_SS_LOADING_MODULE in $libname"

        local libfile=$libname/$_SS_LOADING_MODULE.sh
        if [[ -f $_SS_LIBRARY_ROOT/$libfile ]]
        then
            ss_log "Sourcing $libfile"

            local prior_prefix=$_SS_LOG_PREFIX
            _SS_LOG_PREFIX="> $_SS_LOG_PREFIX"
            
	    . "$_SS_LIBRARY_ROOT/$libfile"

            _SS_LOG_PREFIX=$prior_prefix
            return
        fi
    done

    return 1
}

function ss_load_modules()
{
    ss_log "ss_load_modules $@"
    
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
                ss_log "Seeking script $modname"
                
		_SS_LOADED_MODULES=$_SS_LOADED_MODULES:$modname

                local prior_modname=$_SS_LOADING_MODULE
                _SS_LOADING_MODULE=$modname

                local prior_current=$_SS_CURRENT_LIBRARY
                _SS_CURRENT_LIBRARY=0
                
                if ! ss_source_next
                then
                    ss_log "ERROR: No module named $modname"
                fi

                _SS_LOADING_MODULE=$prior_modname
                _SS_CURRENT_LIBRARY=$prior_current
	esac
    done
}

ss_unset_later ss_load_modules ss_source_next
ss_unset_later _SS_CURRENT_LIBRARY _SS_LOADED_MODULES _SS_LOADING_MODULE



function shellseed_init()
{
    ss_log "Initializing modules: $@"
    ss_log "Effective libraries: ${_SS_LIBRARIES[@]}"

    local modules=$@
    
    if [[ -z $modules ]]
    then
        if [[ -n $PS1 ]]
        then
            modules=interactive
        else
            modules=batch
        fi
    fi
    
    ss_load_modules $modules

    # All done! Remove our stuff from the environment.
    unset $_SS_UNSET_LATER
    unset shellseed_init
}
