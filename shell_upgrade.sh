#!/usr/bin/env bash

if [ -z "$1" ]; then
	echo target not specified
	exit 1
fi
tmux select-pane -t "$1"
tmux send-keys 'python -c ' "'" 'import pty; pty.spawn("/bin/bash")' "'" enter
tmux send-keys C-z
tmux send-keys 'stty raw -echo; fg' enter
tmux send-keys 'export TERM=xterm-256color' enter enter
