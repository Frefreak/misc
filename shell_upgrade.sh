#!/usr/bin/env bash

if [ -z "$1" ]; then
	echo target not specified
	exit 1
fi
tmux select-pane -t "$1"
tmux send-keys 'python -c ' "'" 'import pty; pty.spawn("/bin/bash")' "'" enter
sleep 1
tmux send-keys C-z
sleep 1
tmux send-keys 'stty raw -echo; stty size; fg' enter
sleep 1
tmux send-keys 'export SHELL=bash TERM=xterm-256color' enter enter
