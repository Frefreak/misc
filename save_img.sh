#!/bin/bash

# upload image to vim-cn (https://img.vim.cn.com) and 
# get share url from clipboard

saved_dir="/home/adv_zxy/Pictures"
filename=${saved_dir}/`cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1`".png"

maim -sn ${filename}
if [[ $? -ne 0 ]]
then
        kdialog --title "Fail" --passivepopup "Something bad happened!" 2
        exit
else
        kdialog --title "Success" --passivepopup "Save image successfully!" 2
fi

#xdotool key "ctrl+v"

sleep 30
rm ${filename}
