if type direnv &>/dev/null
then
    if [[ -n $ZSH_VERSION ]]
    then
        eval "$(direnv hook zsh)"
    elif [[ -n $BASH_VERSION ]]
    then
        eval "$(direnv hook bash)"
    else
        ss_log "Unhandled shell; couldn't install direnv hook"
    fi
else
    ss_log "direnv not detected"
fi
