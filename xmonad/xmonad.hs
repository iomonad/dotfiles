{- WM: Xmonad
   Author: IOmonad <iomonad@riseup.net>
   Date: 21:42 26 Nov 2016  -}

-- [IMPORTS] {{{
-- Xmonad: Core
import           XMonad
import           XMonad.Config.Azerty
import qualified XMonad.StackSet as W
-- Haskell: Common
import           System.IO (hPutStrLn)
import           System.Exit (exitSuccess)
import           Data.Maybe (isJust)
import           Data.List
-- Xmonad: Utilies
import           XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import           XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, namedScratchpadAction, customFloating)
import           XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import           XMonad.Util.SpawnOnce
-- Hooks
import           XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, dzenColor, pad, shorten, wrap, PP(..))
import           XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import           XMonad.Hooks.Place (placeHook, withGaps, smart)
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.FloatNext (floatNextHook, toggleFloatNext, toggleFloatAllNew)
-- Actions
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import           XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import           XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import           XMonad.Actions.WithAll (sinkAll, killAll)
import           XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..))
import           XMonad.Actions.GridSelect (GSConfig(..), goToSelected, bringSelected, colorRangeFromClassName, buildDefaultGSConfig)
import           XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.MouseResize
import qualified XMonad.Actions.ConstrainedResize as Sqr
-- Layouts modifiers
import           XMonad.Layout.PerWorkspace (onWorkspace)
import           XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import           XMonad.Layout.WorkspaceDir
import           XMonad.Layout.Spacing (spacing)
import           XMonad.Layout.Minimize
import           XMonad.Layout.Maximize
import           XMonad.Layout.BoringWindows (boringWindows)
import           XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import           XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import           XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import           XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
-- Layouts
import           XMonad.Layout.GridVariants (Grid(Grid))
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.OneBig
import           XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import           XMonad.Layout.IM (withIM, Property(Role))
-- Prompts
import           XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))
-- [/IMPORTS]}}}

-- [SETTINGS] {{{
-- Colors and Styles
sFont    = "xft:PragmataPro:size=6"
sBordW   = 2 -- Set width border size
sColorsB = "#ffffff" -- Unselected terminal
sColorsF = "#3D3D3D" -- Selected Terminal
sColorsW = "#" -- Colors when activity or warning
-- Settings and Other.
myModMask       = mod4Mask -- Set as "SUPER" key akka w1nd0w$
myTerminal      = "urxvtc" -- The terminal to use.
-- Prompt Colors
myPromptConfig =
    defaultXPConfig { font                  = sFont
                    , bgColor               = sColorsB
                    , fgColor               = sColorsF
                    , bgHLight              = sColorsB
                    , fgHLight              = sColorsF
                    , borderColor           = sColorsW
                    , promptBorderWidth     = sBordW
                    , height                = 20
                    , position              = Top
                    , historySize           = 0
                    }
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
    , gs_cellpadding  = 10
    , gs_font         = sFont
    }
-- [/SETTINGS]}}}

-- [SCRATCHPADS] {{{
-- Very handy hotkey-launched floating terminal window.
-- Pressing it will spawn the terminal, or bring it to the
-- current workspace if it already exists.
myScratchpads =
              [ NS "terminal" "urxvtc -name terminal -e tmux attach"     (resource =? "terminal") myPosition
              , NS "music" "urxvtc -name music -e ncmpcpp"               (resource =? "music")    myPosition
              , NS "rtorrent" "urxvtc -name rtorrent -e rtorrent"        (resource =? "rtorrent") myPosition
              , NS "ide" "urxvtc -name ide  -e emacs"                    (resource =? "ide")      myPosition
              ] where myPosition = customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3)
-- [/SCRATCHPADS] }}}

-- [KeyBindings] {{{
myKeys =
    -- Xmonad
        [ ("M-C-r",             spawn "xmonad --recompile") -- Recompile source code
        , ("M-M1-r",            spawn "xmonad --restart") -- Restart fresh binary
        , ("M-S-Esc",            io exitSuccess) -- Exit XMonad

    -- Windows
        -- Core
        , ("M-r",               refresh) -- Refresh layout
        , ("M-q",               kill1) -- Kill current props
        , ("M-C-q",             killAll) -- Full cleanup. Kill all props on the layout.
        -- Utils
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
        -- Maximisation
        , ("M-*",               withFocused minimizeWindow)
        , ("M-S-*",             sendMessage RestoreNextMinimizedWin)
        , ("M-!",               withFocused (sendMessage . maximizeRestore))
        , ("M-$",               toggleFloatNext)
        , ("M-S-$",             toggleFloatAllNew)
        , ("M-S-s",             windows copyToAll)
        , ("M-C-s",             killAllOtherCopies)
        -- Moving and arrangement
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
        , ("M-S-f",             sendMessage (T.Toggle "float")) -- Toggle float layou mode
        , ("M-S-x",             sendMessage $ Toggle REFLECTX)
        , ("M-S-y",             sendMessage $ Toggle REFLECTY)
        , ("M-S-m",             sendMessage $ Toggle MIRROR)
        , ("M-S-b",             sendMessage $ Toggle NOBORDERS)
        , ("M-S-d",             sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
        , ("M-<KP_Multiply>",   sendMessage (IncMasterN 1))
        , ("M-<KP_Divide>",     sendMessage (IncMasterN (-1)))
        , ("M-S-<KP_Divide>",   decreaseLimit)
        , ("M-S-<KP_Multiply>", increaseLimit)
    -- Shrinks and Zooms
        , ("M-h",               sendMessage Shrink)
        , ("M-l",               sendMessage Expand)
        , ("M-k",               sendMessage zoomIn)
        , ("M-j",               sendMessage zoomOut)
        , ("M-S-;",             sendMessage zoomReset)
        , ("M-;",               sendMessage ZoomFullToggle)
    -- Workspace
        , ("<KP_Add>",          moveTo Next nonNSP)
        , ("<KP_Subtract>",     moveTo Prev nonNSP)
        , ("M-<KP_Add>",        moveTo Next nonEmptyNonNSP)
        , ("M-<KP_Subtract>",   moveTo Prev nonEmptyNonNSP)
        , ("M-S-<KP_Add>",      shiftTo Next nonNSP >> moveTo Next nonNSP)
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
        , ("M-M1-<KP_Add>",     addWorkspacePrompt myPromptConfig)
        , ("M-M1-<KP_Subtract>",removeEmptyWorkspace)
    -- Prompts
        , ("M-,",               goToSelected $ myGSConfig myGridConfig)
        , ("M-S-,",             bringSelected $ myGSConfig myGridConfig)
        , ("M-:",               changeDir myPromptConfig)
    -- Scratchpads
        , ("M-<Tab>",           namedScratchpadAction myScratchpads "terminal")
        , ("M-c",               namedScratchpadAction myScratchpads "wcalc")
        , ("M-b",               namedScratchpadAction myScratchpads "rtorrent")
        , ("M-n",               namedScratchpadAction myScratchpads "ide")
    -- Apps
        , ("M-<Space>",         spawn "rofi -show run") -- Start Rofi
        , ("M-<Return>",        spawn "urxvtc_mod -name urxvt") -- New terminal Instance
        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
myMouseKeys = [ ((mod4Mask .|. shiftMask, button3), \w -> focus w >> Sqr.mouseResizeWindow w True) ] -- Custom extra keys.
-- [/KEYBINDINGS] }}}

-- [WORKSPACE] {{{
myWorkspaces = [" i", "ii", "iii", "iv","v"] -- Define numbers and names of workspaces
myManageHook = placeHook (withGaps (14,2,2,2) (smart (0.5,0.5))) <+> insertPosition End Newer <+> floatNextHook <+> namedScratchpadManageHook myScratchpads <+>
        (composeAll . concat $
        [ [ resource  =? r --> doF (W.view " i" . W.shift " i")   | r <- myTermApps    ]
        , [ resource  =? r --> doF (W.view "ii" . W.shift "ii")   | r <- myWebApps     ]
        , [ resource  =? r --> doF (W.view "iii" . W.shift "iii") | r <- myMediaApps   ]
        , [ resource  =? r --> doF (W.view "iv" . W.shift "iv")   | r <- mySystApps    ]
        , [ resource  =? r --> doFloat                            | r <- myFloatApps   ]
        , [ className =? c --> ask >>= doF . W.sink               | c <- myUnfloatApps ]
        ]) <+> manageHook defaultConfig
        where
            myTermApps    = ["urxvt", "xterm", "xfce4-terminal", "xfontsel"]
            myWebApps     = ["Navigator", "newsbeuter", "mutt", "luakit", "midori", "Mail", "dwb"]
            myMediaApps   = ["easytag", "sonata", "comix", "inkscape", "vlc", "zathura", "gnome-mplayer", "Audacity", "hotot", "ncmpcpp", "weechat", "mplayer", "gimp", "gimp-2.8"]
            mySystApps    = ["ranger", "thunar", "Thunar", "lxappearance", "geany", "nitrogen", "Qt-subapplication", "gparted", "bleachbit"]

            myFloatApps   = ["Dialog", "htop", "file-roller", "nitrogen", "display", "feh", "xmessage", "trayer"]
            myUnfloatApps = ["Gimp"]
-- [/WORKSPACE] }}}

-- [LAYOUTS] {{{
--Layouts definitions, defined in differents workspaces.
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts float $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ renamed [CutWordsLeft 4] $ maximize $ minimize $ boringWindows $ spacing 14 $
                onWorkspace " i"  myTermLayout  $
                onWorkspace "ii"  myWebLayout   $
                onWorkspace "iii" myMediaLayout $
                onWorkspace "iv"  mySystLayout
                myDefaultLayout
    where
        myTermLayout    = workspaceDir "~/"                 $ oneBig  ||| space ||| lined ||| grid
        myWebLayout     = workspaceDir "~/download"         $ monocle ||| oneBig ||| space ||| lined
        myMediaLayout   = workspaceDir "~/media/movies"     $ oneBig ||| space ||| lined
        mySystLayout    = workspaceDir "~/"                 $ lined ||| oneBig ||| space ||| monocle ||| grid
        myDefaultLayout = workspaceDir "~/"                 $ float ||| oneBig ||| space ||| lined ||| monocle ||| grid
        oneBig          = renamed [Replace "oneBig"]       $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
        space           = renamed [Replace "space"]        $ limitWindows 4  $ spacing 36 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
        lined           = renamed [Replace "lined"]        $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
        monocle         = renamed [Replace "monocle"]      $ limitWindows 20   Full
        grid            = renamed [Replace "grid"]         $ limitWindows 12 $ mkToggle (single MIRROR) $ Grid (16/10)
        float           = renamed [Replace "float"]        $ limitWindows 20   simplestFloat
-- [/LAYOUTS] }}}

-- [AUTOSTART] {{{
-- Start some program at xsession startup
myStartupHook = do
      --  spawnOnce "mpd &"
          spawnOnce "unclutter &"
          spawnOnce "hsetroot -fill ~/.xres.d/wallpapers/jenga.jpg"
          spawnOnce "compton -c -b -e 0.8 -t -8 -l -9 -r 6 -o 0.7 -m 1.0 &"
          spawnOnce "urxvtc -name terminal -e tmux &"
-- [/AUTOSTART] }}}

-- [MAIN] {{{
-- Centralized configuration entry
main = do
    xmonad       $  azertyConfig
        { modMask            = myModMask
        , terminal           = myTerminal
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
   --   , logHook            = myLogHook dzenLeftBar >> updatePointer (Relative 0.5 0.5)
        , startupHook        = myStartupHook
        , workspaces         = myWorkspaces
        , borderWidth        = sBordW
        , normalBorderColor  = sColorsB
        , focusedBorderColor = sColorsW
        } `additionalKeysP`         myKeys
          `additionalMouseBindings` myMouseKeys
-- [/MAIN] }}}
