ss_source_next

# On Apple Silicon, /usr/local/bin isn't in default PATH 
pathmunge /usr/local/bin after

# On Apple Silicon, Homebrew is no longer in /usr/local
[[ -d /opt/homebrew/bin ]] && pathmunge /opt/homebrew/bin before

# TODO Move pathmunge into Shellseed script.
#   Homebrew should follow ~/etc/bin but we can't add it yet since pathmunge
#   is defined in the next library layer.
