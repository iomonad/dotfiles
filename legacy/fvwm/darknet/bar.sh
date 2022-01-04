#!/bin/bash
conky -c conkyrc \
    | lemonbar -bd \
	       -f "profont:10" \
	       -B $(xrdb -query | grep "*background" | cut -f 2) \
	       -F $(xrdb -query | grep "*foreground" | cut -f 2) &
