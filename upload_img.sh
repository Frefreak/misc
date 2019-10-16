#!/bin/bash

# upload image to vim-cn (https://img.vim.cn.com) and 
# get share url from clipboard

saved_dir="/tmp/"
server="https://img.vim-cn.com/"
filename=${saved_dir}`cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1`".png"

maim -sn ${filename}
if [[ $? -ne 0 ]]
then
        exit
fi

fileurl=`proxychains -q curl -sF name=@${filename} ${server}`
echo ${fileurl} | grep "https" | grep "png" > /dev/null
if [[ $? -eq 0 ]]
then
        echo $fileurl | xclip -i -selection clipboard
        kdialog --title "Success" --passivepopup "Got image url successfully!" 2
else
        kdialog --title "Fail" --passivepopup "Something bad happened!" 2
fi
#xdotool key "ctrl+v"
rm ${filename}
