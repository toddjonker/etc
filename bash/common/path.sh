# Basic path setup

ss_log "[path] entry PATH=$PATH"

# Taken from /etc/profile
pathmunge () {
    ss_log "[path] pathmunge" "$@"
    
    case ":${PATH}:" in
        *:"$1":*)
            ;;
        *)
            if [ "$2" = "after" ] ; then
                PATH=$PATH:$1
            else
                PATH=$1:$PATH
            fi
    esac
}
ss_unset_later pathmunge

# ~/bin precedes ~/etc/bin so you can override binaries in the library.

if [[ -d $USER_LIBRARY/bin ]]
then
    pathmunge "$USER_LIBRARY/bin" before
fi

if [[ -d ~/.local/bin ]]
then
    pathmunge ~/.local/bin before
fi

if [[ -d ~/bin ]]
then
    pathmunge ~/bin before
fi

ss_source_if_present ~/.cargo/env
