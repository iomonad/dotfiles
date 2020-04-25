#!/bin/bash
clear

WIDTH=`tput cols`
HEIGHT=`tput lines`
h=`expr $HEIGHT / 2`
NUMBARS=8
BARWIDTH=`expr $WIDTH / $NUMBARS`

l="1"

while [ "$l" -lt $HEIGHT ]; do
#while [ "$l" -lt $h ]; do
    b="0"
    while [ "$b" -lt $NUMBARS ]; do
        s="0"
        while [ "$s" -lt $BARWIDTH ]; do
            echo -en "\e[3"$b"m█"
            s=`expr $s + 1`
        done
        b=`expr $b + 1`
    done
#        echo
    l=`expr $l + 1`
    echo
done
