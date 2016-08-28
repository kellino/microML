#!/usr/bin/bash

ml=$(which microML)
FILES="../test/compiler/*.mml"

for f in $FILES
do 
    filename=$(basename "$f")
    newfile="${filename%.*}"
    if [ -x "$ml" ]; then
        printf "attempting to compile \e[1m%s\e[0m\n" "$filename" >> output.txt
        microML -c "$f" "../test/compiler/res/$newfile" >> output.txt
    else
        echo "\e[31mError:\e[0m microML is not in your system path" 
        break
    fi
done 
