#!/usr/bin/env fish

set term_exec alacritty
set nvim_exec nvim
set server_path "$HOME/.cache/nvim/godot-server.pipe"
set filename (echo "$argv[1]" | string replace " " "\ ")

function start_server
	$term_exec -e "$nvim_exec" --listen "$server_path" "$argv[1]"
end

function open_file_in_server
	$nvim_exec --server "$server_path" --remote-send "<C-\><C-n>;n $argv[1]<CR>"
end

if ! test -e "$server_path"
	start_server "$argv[1]"
else 
	open_file_in_server "$argv[1]"
end
