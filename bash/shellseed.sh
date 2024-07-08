###   Copyright (C) 2017-2023 Todd V. Jonker.  All Rights Reserved.
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
[[ -r "$USER_LIBRARY" ]] || echo "Bad USER_LIBRARY=$USER_LIBRARY"


#------------------------------------------------------------------------------
# Cleanup mechanism

# This array holds the names of items to be unset when we're done.
declare -a _SS_UNSET_LATER

function ss_unset_later()
{
    _SS_UNSET_LATER=("$@" "${_SS_UNSET_LATER[@]}")
}

function ss_cleanup()
{
    # All done! Remove our stuff from the environment.
    ss_log "Cleaning up the shell environment"    

    for v in "${_SS_UNSET_LATER[@]}"
    do
	    unset "$v"
    done
}

ss_unset_later _SS_UNSET_LATER ss_unset_later ss_cleanup


#------------------------------------------------------------------------------
# Logging

function ss_log()
{
    if [[ -n "$SHELLSEED_LOG" ]]
    then
      local IFS=' '
      echo "$_SS_LOG_PREFIX$*" >> "$SHELLSEED_LOG"
    fi
}

ss_unset_later ss_log SHELLSEED_LOG _SS_LOG_PREFIX

ss_log "---------------------"
ss_log "$(date)"
ss_log "Current PATH=$PATH"


#------------------------------------------------------------------------------
# Module loading

# TODO This should be a command-line argument.
_SS_LIBRARY_ROOT=${USER_LIBRARY}/bash

declare -a _SS_LIBRARIES

function shellseed_use_libraries()
{
    # Process the arguments, recursively, right to left.
    # Leftmost library has priority when searching for modules.
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
            ss_log "Registering library $libname"

            local libdir=$_SS_LIBRARY_ROOT/$libname
            if [[ -d $libdir ]]
            then
                local libfile=$libdir/use-libraries
                if [[ -f $libfile ]]
                then
                    # shellcheck disable=SC2046
                    shellseed_use_libraries $(cat "$libfile")
                fi

                _SS_LIBRARIES=("$libname" "${_SS_LIBRARIES[@]}")
            else
                ss_log "ERROR: No library named $libname in $_SS_LIBRARY_ROOT"
            fi
            ;;
    esac
}

ss_unset_later shellseed_use_libraries _SS_LIBRARIES _SS_LIBRARY_ROOT


#
# Find library file with shell-appropriate extension.
#
function _ss_pick_libfile()
{
    local basename=$1
    local basepath=$_SS_LIBRARY_ROOT/$basename

    if [[ -n $ZSH_VERSION && -f $basepath.zsh ]]
    then
        echo "$basename.zsh"
    elif [[ -n $BASH_VERSION && -f $basepath.bash ]]
    then
        echo "$basename.bash"
    else
        echo "$basename.sh"
    fi
}

ss_unset_later _ss_pick_libfile


function ss_source_if_present()
{
    local script=$1
    local base=$2

    if [[ -f "${base}${script}" ]]
    then
        ss_log "Sourcing $script"

        local prior_prefix=$_SS_LOG_PREFIX
        _SS_LOG_PREFIX="> $_SS_LOG_PREFIX"

        # shellcheck disable=SC1090
        source "${base}${script}"

        _SS_LOG_PREFIX=$prior_prefix
    else
        ss_log "Not sourcing absent file $script"
    fi
}
ss_unset_later ss_source_if_present


function ss_source_next()
{
    # This uses global variables to make it easy on the user.

    local libfile # Zsh prints current value, so don't do this in the loop.
    
    while (( _SS_CURRENT_LIBRARY < ${#_SS_LIBRARIES[*]} ))
    do
        # Zsh arrays start at 1, but comparison above assumes 0, so pre-increment.
        [[ -n $ZSH_VERSION ]] && _SS_CURRENT_LIBRARY=$(( _SS_CURRENT_LIBRARY + 1 ))

        local libname=${_SS_LIBRARIES[$_SS_CURRENT_LIBRARY]}

        [[ -z $ZSH_VERSION ]] && _SS_CURRENT_LIBRARY=$(( _SS_CURRENT_LIBRARY + 1 ))

        ss_log "Looking for $_SS_LOADING_MODULE in $libname"

        libfile=$(_ss_pick_libfile "$libname/$_SS_LOADING_MODULE")
        if [[ -f $_SS_LIBRARY_ROOT/$libfile ]]
        then
            ss_source_if_present "$libfile" "$_SS_LIBRARY_ROOT/"
            return
        fi
    done

    return 1
}

function ss_load_modules()
{
    ss_log "ss_load_modules" "$@"
    
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
    ss_log "Effective libraries:" "${_SS_LIBRARIES[@]}"

    local modules=("$@")
    ss_log "Requested modules: ${modules[*]}"

    if [[ $# -eq 0 ]]
    then
        if [[ -n $PS1 ]]
        then
            modules=("interactive")
        else
            modules=("batch")
        fi
    fi
    
    ss_load_modules "${modules[@]}"
    ss_cleanup
}
ss_unset_later shellseed_init


declare -a _SS_MODULES

function _ss_parse_args()
{
    local OPTARG OPTIND o
    local -a libs

    while getopts ":l:m:h" o; do
        case "$o" in
            h) _ss_usage; return;;
            l) libs=("$OPTARG" "${libs[@]}");;
            m) _SS_MODULES=("$OPTARG" "${_SS_MODULES[@]}");;
            *) echo "[shellseed] illegal option -$OPTARG"; _ss_usage; return;;
        esac
    done
    shift $((OPTIND-1))
    unset OPTARG OPTIND

    [[ ${#libs} -eq 0 ]] && libs=(common)

    shellseed_use_libraries "${libs[@]}"

    # Cannot load modules here; we first need to cleanup the environment from getopts!
}
ss_unset_later _ss_parse_args _SS_MODULES


function _ss_usage()
{
    echo
    echo "Usage: . shellseed.sh [-l LIBRARY] [-m MODULE]"
    echo
    echo "Loads the given modules from the libraries."
    echo "The -l and -m options may be given multiple times."
    echo "When no library is specified, the default is \"common\"."
    echo "When no modules are requested, then either \"interactive\" or \"batch\" is loaded,"
    echo "depending on the shell's status."
}
ss_unset_later _ss_usage


# When arguments are given, parse them.
# Otherwise, assume the caller will invoke shellseed_init later.
[[ -n "$*" ]] && _ss_parse_args "$@" && shellseed_init "${_SS_MODULES[@]}"
