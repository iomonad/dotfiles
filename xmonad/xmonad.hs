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
import qualified Data.Map as M
import           Data.Bits ((.|.))
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
import           XMonad.Actions.CycleWS (prevWS, nextWS, moveTo, shiftTo, WSType(..))
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
import           XMonad.Layout.GridVariants (Grid(Grid))
import           XMonad.Layout.OneBig
import           XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import           XMonad.Layout.IM (withIM, Property(Role))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Gaps
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Circle
import           XMonad.Layout.ThreeColumns
-- Prompts
import           XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))
-- [/IMPORTS]}}}

-- [SETTINGS] {{{
-- Colors and Styles
sFont    = "-*-lemon-*-*-*-*-*-*-*-*-*-*-*-*"
sBordW   = 5 -- Set width border size
sColorsB = "#12AEF" -- Unselected terminal
sColorsF = "#ffffff" -- Selected Terminal
sColorsW = "#ffffff" -- Colors when activity or warning

sWall :: String
sWall = "jenga.jpg" -- The wallpaper in $HOME/media/images/wallpapers/

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
    (0x00,0x00,0x00) -- lowest inactive bg
    (0xBB,0xAA,0xFF) -- highest inactive bg
    (0x88,0x66,0xAA) -- active bg
    (0xBB,0xBB,0xBB) -- inactive fg
    (0x00,0x00,0x00) -- active fg
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
              , NS "music" "urxvtc -name music -e tmux -c ncmpcpp"               (resource =? "music")    myPosition
              , NS "rtorrent" "urxvtc -name rtorrent -e rtorrent"        (resource =? "rtorrent") myPosition
              , NS "ide" "urxvtc -name ide  -e emacs"                    (resource =? "ide")      myPosition
              ] where myPosition = customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3)
-- [/SCRATCHPADS] }}}

-- [KeyBindings] {{{
myKeys =  -- The Workspace switcher.
    {- Keybindings methodologies:
     - Meta -> Only for for first action
     - Shift -> Secondaries
     - Alt -> Special -}
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
        , ("M-<Delete>",        withFocused $ windows . W.sink) -- Rotate windows
        , ("M-S-<Delete>",      sinkAll) -- Toggle border selection
        -- Windows selection (ALT - M1 based)
        , ("M1-z",               windows W.focusMaster) -- Select master
        , ("M1-<F9>",           windows W.focusDown) -- Select using mouse hovering
        , ("M1-<Tab>",          windows W.focusDown) -- Select using simple term selection switcher.
        , ("M1-a",              windows W.swapDown) -- It say: swap down
        , ("M1-e",              windows W.swapUp) -- Same for up, no ?
        , ("M1-S-<Tab>",        rotSlavesDown) -- Select using rotation tree 1
        , ("M1-C-<Tab>",        rotAllDown) -- Select using rotation tree O
        , ("M1-<Backspace>",    promote) --  Promote to replace master layout (terminal)
        -- Maximisation
        , ("M-$",               withFocused minimizeWindow) -- Minimize windows to trails
        , ("M-S-$",             sendMessage RestoreNextMinimizedWin) -- Restore last minimized windows
        -- Windows manipulation ( Shift to increase, Control to decrease and None to move)
        , ("M-<Up>",            sendMessage (MoveUp 10)) -- Simple move to upper
        , ("M-<Down>",          sendMessage (MoveDown 10)) -- Simple move to down
        , ("M-<Right>",         sendMessage (MoveRight 10)) -- Simple move to right
        , ("M-<Left>",          sendMessage (MoveLeft 10)) -- Simple Move to left
        , ("M-S-<Up>",          sendMessage (IncreaseUp 10)) -- Increase size to up
        , ("M-S-<Down>",        sendMessage (IncreaseDown 10)) -- Increase size to down
        , ("M-S-<Right>",       sendMessage (IncreaseRight 10)) -- Increase size to right
        , ("M-S-<Left>",        sendMessage (IncreaseLeft 10)) -- Increase size to left
        , ("M-C-<Up>",          sendMessage (DecreaseUp 10)) -- Descrease size to up
        , ("M-C-<Down>",        sendMessage (DecreaseDown 10)) -- Descrease size to down
        , ("M-C-<Right>",       sendMessage (DecreaseRight 10)) -- Decrease size to right
        , ("M-C-<Left>",        sendMessage (DecreaseLeft 10)) -- Descrease size to left
    -- Layouts
        , ("M-!",               asks (XMonad.layoutHook . config) >>= setLayout) -- Reset all layout modifications.
        , ("M-*",                sendMessage NextLayout) -- Rotate between different layouts.
        , ("M-S-f",             sendMessage (T.Toggle "float")) -- Toggle float layou mode
        , ("M-S-b",             sendMessage $ Toggle NOBORDERS) -- Toggle border display
        , ("M-S-d",             sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toogle FullScreen
    -- Shrinks
        , ("M-h",               sendMessage Shrink) -- Shrink size of current terminal to left
        , ("M-l",               sendMessage Expand) --  Expand size of current terminal to right
    -- Workspace
        , ("M-M1-<Tab>",        nextWS) -- Switch to next workspace
        , ("M-M1-*",            prevWS) -- Switch to last workspace
        , ("M-S-<KP_Add>",      shiftTo Next nonNSP >> moveTo Next nonNSP) -- Move and follow prop to next workspace
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP) -- Move and follow prop to last workspace
    -- Prompts Popup
        , ("M-,",               goToSelected $ myGSConfig myGridConfig) -- Prompt the popup, and when selected go to the prop
        , ("M-S-,",             bringSelected $ myGSConfig myGridConfig) -- Prompt the popup, and when selected move prop to current workspace
    -- Scratchpads
        , ("M-<Tab>",           namedScratchpadAction myScratchpads "terminal") -- Pop a terminal as scratchpads ^ useless
        , ("M-c",               namedScratchpadAction myScratchpads "ide") -- Start a terminal with emacs for dev
        , ("M-b",               namedScratchpadAction myScratchpads "rtorrent")
        , ("M-m",               namedScratchpadAction myScratchpads "music")
    -- Apps
        , ("M-<Space>",         spawn "rofi -show run") -- Start Rofi
        , ("M-<Return>",        spawn "urxvtc -name urxvt") -- New terminal Instance
        , ("M-w",               spawn "firefox") -- Start firefox
        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
myMouseKeys = [ ((mod4Mask .|. shiftMask, button3), \w -> focus w >> Sqr.mouseResizeWindow w True) ] -- Custom extra keys.
-- [/KEYBINDINGS] }}}

-- [WORKSPACE] {{{
myWorkspaces = ["term", "web", "media", "pirate","porn"] -- Define numbers and names of workspaces
-- This hooks force the redirection to a given workspace.
myManageHook = placeHook (withGaps (5,2,2,2) (smart (0.5,0.5))) <+> insertPosition End Newer <+> floatNextHook <+> namedScratchpadManageHook myScratchpads <+>
        (composeAll . concat $
        [ [ resource  =? r --> doF (W.view "term" . W.shift "term")   | r <- myTermApps    ]
        , [ resource  =? r --> doF (W.view "web" . W.shift "web")   | r <- myWebApps     ]
        , [ resource  =? r --> doF (W.view "media" . W.shift "media") | r <- myMediaApps   ]
        , [ resource  =? r --> doF (W.view "pirate" . W.shift "pirate")   | r <- mySystApps    ]
        , [ resource  =? r --> doFloat                            | r <- myFloatApps   ] -- Make float apss floating
        , [ className =? c --> ask >>= doF . W.sink               | c <- myUnfloatApps ]
        ]) <+> manageHook defaultConfig
        where
            myTermApps    = [] -- Removing urxvt to avoid workspace redirections.
            myWebApps     = ["firefox"]
            myMediaApps   = ["zathura","mplayer"]
            mySystApps    = []
            myFloatApps   = ["Dialog","lxappearance"]
            myUnfloatApps = []
-- [/WORKSPACE] }}}
--
-- [LAYOUTS] {{{
--Layouts definitions, defined in differents workspaces.
myLayoutHook = gaps [(U, 8), (R, 8), (L, 8), (D, 8)] $
                                         avoidStruts $
                                         spacing 8
                                         commonLayouts
     where commonLayouts = tiled ||| grid ||| oneBig ||| lined ||| space ||| monocle
           -- Layout defined (Custom)
           monocle = limitWindows 20 Full -- Fullpaged
           -- Default tiling layout
           tiled   = Tall nmaster delta ratio
           -- One Big Layout, other more smaller
           oneBig  = limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $
                            mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $
                                  OneBig (2/3) (2/3) -- 2 on 3 ratio.
           -- Same as oneBig, but with ultra large gap
           space   = limitWindows 4  $ spacing 36 $ Mirror $
                            mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $
                                    mkToggle (single REFLECTY) $ OneBig (2/3) (2/3) -- Adjust to big gap
           -- Grided like we like, w/ batteries included
           grid    = limitWindows 12 $ mkToggle (single MIRROR) $ Grid (16/10)
           -- Linear like kit-kats / Cannot be grided
           lined   = limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
           -- Variables
           nmaster = 1
           ratio   = 1/2
           delta   = 3/100

-- [/LAYOUTS] }}}

-- [AUTOSTART] {{{
-- Start some program at xsession startup
myStartupHook = do
      --  spawnOnce "mpd &"
          spawnOnce "wmname LG3D"
          spawnOnce $ "hsetroot -fill ~/media/images/wallpapers/" ++ sWall
          spawnOnce "unclutter &"
          spawnOnce "compton -c -b -e 0.8 -t -8 -l -9 -r 6 -o 0.7 -m 1.0 &"
          spawnOnce "urxvtc -e tmux &"
-- [/AUTOSTART] }}}

-- [MAIN] {{{
-- Centralized configuration entry
main = xmonad       $  azertyConfig
        { modMask            = myModMask
        , terminal           = myTerminal
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , startupHook        = myStartupHook
        , workspaces         = myWorkspaces
        , borderWidth        = sBordW
        , normalBorderColor  = sColorsB
        , focusedBorderColor = sColorsW
        } `additionalKeysP`         myKeys
          `additionalMouseBindings` myMouseKeys
-- [/MAIN] }}}
