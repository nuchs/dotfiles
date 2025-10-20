#!/usr/bin/env bash

paru -Sl $@ |
awk '{print $1, $2, ($3=="[installed]"?$3:"")}' |
fzf --nth=2 --with-nth=1,2,3 \
    --prompt='pkg> ' \
    --bind 'enter:execute(echo "sudo paru -S {2}")+abort' \
    --preview 'paru -Si {2}; echo; echo "Files (if installed):"; paru -Ql {2} 2>/dev/null | sed "s/^/  /"' \
    --preview-window='down,60%,wrap'
