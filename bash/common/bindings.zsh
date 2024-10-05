# ZSH Key Bindings

# https://zsh.sourceforge.io/Doc/Release/Zsh-Line-Editor.html

# ^[[ == ESC [ == xterm Control Sequence Identifier (CSI)
# https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Controls-beginning-with-ESC

# The CSIs below are VT220-style function keys
# https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-VT220-Style-Function-Keys

bindkey '^[[1~' beginning-of-line         # home key
bindkey '^[[2~' overwrite-mode            # insert key
bindkey '^[[3~' delete-char-or-list       # del key
bindkey '^[[4~' end-of-line               # end key
bindkey '^[[5~' up-line-or-history        # pgup key
bindkey '^[[6~' down-line-or-history      # pgdn key


# These are "PC-style" variants. For some reason these are issued on macOS.
# https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-PC-Style-Function-Keys

bindkey '^[[H' beginning-of-line          # home key
bindkey '^[[F' end-of-line                # end key
