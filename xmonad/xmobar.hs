Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , border = BottomB
       , borderColor = "black"
       , position = Static { xpos = 0 , ypos = 0, width = 1920, height = 15 }
       , lowerOnStart =     True    -- send to bottom of window stack on start
       , allDesktops =      True    -- show on all desktops
       , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
       , pickBroadest =     False   -- choose widest display (multi-monitor)
       , persistent = True          -- enable/disable hiding (True = disabled)
       , iconRoot = "$HOME/.icons"
       ,hideOnStart = False
       , commands = [ Run Com "weather" [] "" 3600
                    , Run Network "wlp5s0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Com "whoami" [] "" 3600
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run ThermalZone 0 ["-t","Sys: <temp>°"] 30
                    , Run StdinReader
       ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }{ %cpu% | %memory% * %swap% | %thermal0% | %weather% | %whoami%@%uname% | <fc=#ee9a00>%date%</fc>"
}
