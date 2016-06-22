--   [ xmonad.hs -- 12-Jun-2016 ]    --
--             seytz (c)             --

-- GHC 7.10.3, If you have some errors,
-- please execute:
-- $ sudo emerge xmonad-contrib

--  Haskell is pretty funtional and  --
--  pretty lazy. With some docs we   --
--  can build an awesome workflow,   --
--          take a look ...          --

    -- Base
import XMonad
import Data.Maybe (isJust)
import Data.List
import XMonad.Config.Azerty
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)  
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, namedScratchpadAction, customFloating)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, dzenColor, pad, shorten, wrap, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FloatNext (floatNextHook, toggleFloatNext, toggleFloatAllNew)

    -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..)) 
import XMonad.Actions.GridSelect (GSConfig(..), goToSelected, bringSelected, colorRangeFromClassName, buildDefaultGSConfig)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace) 
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir 
import XMonad.Layout.Spacing (spacing) 
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))

    -- Prompts
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))
                                  --------------------

    -- Styles
myFont          = "terminus"
myBorderWidth   = 4
myColorBG       = "#181512"
myColorWhite    = "#683e3e"
myColorRed      = "#cd546c"
myColorBrown    = "#989584"

    -- Settings
myModMask       = mod4Mask
myTerminal      = "urxvt"



    -- Grid selector colors
myGridConfig = colorRangeFromClassName
    (0x18,0x15,0x12) -- lowest inactive bg
    (0x18,0x15,0x12) -- highest inactive bg
    (0x18,0x15,0x12) -- active bg
    (0x98,0x95,0x84) -- inactive fg
    (0xcd,0x54,0x6c) -- active fg

myGSConfig colorizer  = (buildDefaultGSConfig myGridConfig)
    { gs_cellheight   = 65
    , gs_cellwidth    = 120
    , gs_cellpadding  = 0
    , gs_font         = myFont
    }

--scratchpads
myScratchpads = 
              [ NS "terminal" "urxvtc_mod -name terminal -e tmux attach"     (resource =? "terminal") myPosition
              , NS "music" "urxvtc_mod -name music -e ncmpcpp"               (resource =? "music")    myPosition
              , NS "rtorrent" "urxvtc_mod -name rtorrent -e rtorrent"        (resource =? "rtorrent") myPosition
              , NS "wcalc" "urxvtc_mod -name wcalc -e wcalc"                 (resource =? "wcalc")    myPosition
              ] where myPosition = customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3)

-- keybind
myKeys =
    -- Xmonad
        [ ("M-C-r",             spawn "xmonad --recompile")
        , ("M-M1-r",            spawn "xmonad --restart")
        , ("M-S-r",             spawn "pkill dzen && xmonad --restart")
        , ("M-M1-q",            io exitSuccess)
    
    -- Windows
        , ("M-r",               refresh)
        , ("M-q",               kill1)
        , ("M-C-q",             killAll)
        , ("M-S-q",             killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP)

        , ("M-<Delete>",        withFocused $ windows . W.sink)
        , ("M-S-<Delete>",      sinkAll)
        , ("M-z",               windows W.focusMaster)
        , ("M1-<F9>",           windows W.focusDown) -- Mouse special button
        , ("M1-<Tab>",          windows W.focusDown)
        , ("M-a",               windows W.swapDown)
        , ("M-e",               windows W.swapUp)
        , ("M1-S-<Tab>",        rotSlavesDown)
        , ("M1-C-<Tab>",        rotAllDown)
        , ("M-<Backspace>",     promote)

        , ("M-*",               withFocused minimizeWindow)
        , ("M-S-*",             sendMessage RestoreNextMinimizedWin)
        , ("M-!",               withFocused (sendMessage . maximizeRestore))
        , ("M-$",               toggleFloatNext)
        , ("M-S-$",             toggleFloatAllNew)
        , ("M-S-s",             windows copyToAll) 
        , ("M-C-s",             killAllOtherCopies) 

        , ("M-C-M1-<Up>",       sendMessage Arrange)
        , ("M-C-M1-<Down>",     sendMessage DeArrange)
        , ("M-<Up>",            sendMessage (MoveUp 10))
        , ("M-<Down>",          sendMessage (MoveDown 10))
        , ("M-<Right>",         sendMessage (MoveRight 10))
        , ("M-<Left>",          sendMessage (MoveLeft 10))
        , ("M-S-<Up>",          sendMessage (IncreaseUp 10))
        , ("M-S-<Down>",        sendMessage (IncreaseDown 10))
        , ("M-S-<Right>",       sendMessage (IncreaseRight 10))
        , ("M-S-<Left>",        sendMessage (IncreaseLeft 10))
        , ("M-C-<Up>",          sendMessage (DecreaseUp 10))
        , ("M-C-<Down>",        sendMessage (DecreaseDown 10))
        , ("M-C-<Right>",       sendMessage (DecreaseRight 10))
        , ("M-C-<Left>",        sendMessage (DecreaseLeft 10))

    -- Layouts
        , ("M-S-<Space>",       sendMessage ToggleStruts)
        , ("M-d",               asks (XMonad.layoutHook . config) >>= setLayout)
        , ("M-<KP_Enter>",      sendMessage NextLayout)
        , ("M-S-f",             sendMessage (T.Toggle "float"))
        , ("M-S-g",             sendMessage (T.Toggle "gimp"))
        , ("M-S-x",             sendMessage $ Toggle REFLECTX)
        , ("M-S-y",             sendMessage $ Toggle REFLECTY)
        , ("M-S-m",             sendMessage $ Toggle MIRROR)
        , ("M-S-b",             sendMessage $ Toggle NOBORDERS)
        , ("M-S-d",             sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
        , ("M-<KP_Multiply>",   sendMessage (IncMasterN 1))
        , ("M-<KP_Divide>",     sendMessage (IncMasterN (-1)))
        , ("M-S-<KP_Divide>",   decreaseLimit)
        , ("M-S-<KP_Multiply>", increaseLimit)

    -- windows hacks
        , ("M-h",               sendMessage Shrink)
        , ("M-l",               sendMessage Expand)
        , ("M-k",               sendMessage zoomIn)
        , ("M-j",               sendMessage zoomOut)
        , ("M-S-;",             sendMessage zoomReset)
        , ("M-;",               sendMessage ZoomFullToggle)

    -- Workspaces
        , ("<KP_Add>",          moveTo Next nonNSP)
        , ("<KP_Subtract>",     moveTo Prev nonNSP)
        , ("M-S-<KP_Add>",        moveTo Next nonEmptyNonNSP)
        , ("M-S-<KP_Subtract>",   moveTo Prev nonEmptyNonNSP)
        , ("M-<KP_Add>",      shiftTo Next nonNSP >> moveTo Next nonNSP)
        , ("M-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
        , ("M-M1-<KP_Subtract>",removeEmptyWorkspace)

    -- Apps
        , ("M-<Return>",        spawn "urxvt")
        , ("M-S-<Return>",      spawn "urxvt -e tmux")
        , ("M-<Space>",         spawn "rofi")
        , ("M-g",               spawn "qtox")
     -- , ("M-x",               safeSpawn "i3lock" ["-ubi", "/home/logan/images/accueil.png"])
        , ("M-f",               raiseMaybe (runInTerm "-name ranger" "ranger") (resource =? "ranger"))
        , ("M-t",               raiseMaybe (runInTerm "-name newsbeuter" "newsbeuter") (resource =? "newsbeuter"))
        , ("M-m",               raiseMaybe (runInTerm "-name mutt" "mutt") (resource =? "mutt"))
     -- , ("M-v",               raiseMaybe (runInTerm "-name weechat" "weechat-curses") (resource =? "weechat"))
        , ("M-o",               raiseMaybe (runInTerm "-name htop" "htop") (resource =? "htop"))
        , ("M-w",               runOrRaise "qutebrowser" (resource =? "Navigator"))
        , ("C-n",               spawn "tmux new-window")

    -- Prompts
        , ("M-,",               goToSelected $ myGSConfig myGridConfig)
        , ("M-S-,",             bringSelected $ myGSConfig myGridConfig)

    -- Scratchpads
        , ("M-<Tab>",           namedScratchpadAction myScratchpads "terminal")
        , ("M-c",               namedScratchpadAction myScratchpads "wcalc")
        , ("M-b",               namedScratchpadAction myScratchpads "rtorrent")
        , ("M-n",               namedScratchpadAction myScratchpads "music")
        , ("<XF86Tools>",       namedScratchpadAction myScratchpads "music")

    -- Multimedia Keys
        , ("<XF86AudioPlay>",   spawn "ncmpcpp toggle")
        , ("<XF86AudioPrev>",   spawn "ncmpcpp prev")
        , ("<XF86AudioNext>",   spawn "ncmpcpp next")
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<Print>",           spawn "scrotd 0")
        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

-- workspaces
myWorkspaces = [" i", "ii", "iii", "iv"]
myManageHook = placeHook (withGaps (20,12,12,12) (smart (0.5,0.5))) <+> insertPosition End Newer <+> floatNextHook <+> namedScratchpadManageHook myScratchpads <+>
        (composeAll . concat $
        [ [ resource  =? r --> doF (W.view " i" . W.shift " i")   | r <- myTermApps    ]
        , [ resource  =? r --> doF (W.view "ii" . W.shift "ii")   | r <- myWebApps     ]
        , [ resource  =? r --> doF (W.view "iii" . W.shift "iii") | r <- myMediaApps   ]
        , [ resource  =? r --> doF (W.view "iv" . W.shift "iv")   | r <- mySystApps    ]
        , [ resource  =? r --> doFloat                            | r <- myFloatApps   ]
        , [ className =? c --> ask >>= doF . W.sink               | c <- myUnfloatApps ]
        ]) <+> manageHook defaultConfig
        where
            myTermApps    = ["termite", "xterm"]
            myWebApps     = ["qutebrowser", "newsbeuter", "mutt"]
            myMediaApps   = ["zathura", "ncmpcpp", "weechat", "mplayer","qtox"]
            mySystApps    = ["ranger", "lxappearance", "Qt-subapplication"]

            myFloatApps   = ["Dialog", "htop", "file-roller", "nitrogen", "display", "feh", "xmessage", "trayer"]
            myUnfloatApps = ["Gimp"]


myLayoutHook = avoidStruts $ windowArrange $ T.toggleLayouts float $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ renamed [CutWordsLeft 4] $ maximize $ minimize $ boringWindows $ spacing 0 $
                onWorkspace " i"  myTermLayout  $
                onWorkspace "ii"  myWebLayout   $
                onWorkspace "iii" myMediaLayout $
                onWorkspace "iv"  mySystLayout 
                myDefaultLayout
    where
        myTermLayout    = workspaceDir "~"                 $ oneBig  ||| space ||| lined ||| grid
        myWebLayout     = workspaceDir "~/download" $ monocle ||| oneBig ||| space ||| lined
        myMediaLayout   = workspaceDir "~/media/video"          $ T.toggleLayouts gimp $ monocle ||| oneBig ||| space ||| lined
        mySystLayout    = workspaceDir "~"                 $ lined ||| oneBig ||| space ||| monocle ||| grid
        myDefaultLayout = workspaceDir "/"                 $ float ||| oneBig ||| space ||| lined ||| monocle ||| grid

        oneBig          = renamed [Replace "oneBig"]       $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
        space           = renamed [Replace "space"]        $ limitWindows 4  $ spacing 36 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
        lined           = renamed [Replace "lined"]        $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
        monocle         = renamed [Replace "monocle"]      $ limitWindows 20   Full
        grid            = renamed [Replace "grid"]         $ limitWindows 12 $ mkToggle (single MIRROR) $ Grid (16/10)
        float           = renamed [Replace "float"]        $ limitWindows 20   simplestFloat
        gimp            = renamed [Replace "gimp"]         $ limitWindows 5  $ withIM 0.11 (Role "gimp-toolbox") $ reflectHoriz $ withIM 0.15 (Role "gimp-dock") Full

-- startup & co
myStartupHook = do
          spawnOnce "unclutter &"
          spawnOnce "compton -c -b -e 0.8 -t -8 -l -9 -r 6 -o 0.7 -m 1.0 &"
          spawnOnce "urxvt -e tmux attach || urxvt -e tmux"
main = do
    xmonad       $  azertyConfig
        { modMask            = myModMask
           , terminal           = myTerminal
           , manageHook         = myManageHook 
           , layoutHook         = myLayoutHook 
           , startupHook        = myStartupHook
           , workspaces         = myWorkspaces
           , borderWidth        = myBorderWidth 
           , normalBorderColor  = myColorBG
           , focusedBorderColor = myColorWhite
        } `additionalKeysP`         myKeys 
