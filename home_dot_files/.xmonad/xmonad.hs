import XMonad
import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86()
import System.IO()
import XMonad.Layout.Spacing
import XMonad.Layout.ShowWName()
import XMonad.Layout.Renamed
import XMonad.Layout.Named
import XMonad.Layout.ResizableTile
import XMonad.Actions.MouseResize
import XMonad.Actions.SpawnOn()
import XMonad.Util.SpawnOnce
import XMonad.Util.Run()

-- myNormalBorderColour = "#ebdbb2"
myNormalBorderColour :: String
myNormalBorderColour = "#689D6A"

myFocusedBorderColour :: String
myFocusedBorderColour = "#c050f0"

-- Autostart

-- myStartupHook :: X ()
-- myStartupHook = do
--          spawnOnce "kitty -e tmux &"
--          spawnOnce "~/.fehbg &"
--          spawnOnce "xsetroot -cursor_name left_ptr &"
--          spawnOnce "picom --experimental-backend &"
--          spawnOnce "/usr/bin/dunst &"

------------------------------------------------------------------------
-- Command to launch the bar.
myBar = "xmobar /home/munra/.config/xmobarrc"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#689D6A" "" . wrap " " " "
                -- ppCurrent = xmobarColor "#689D6A" "" . wrap "|" "|"
                , ppTitle = xmobarColor "#689D6A" "" . shorten 40     -- Title of active window in xmobar
                , ppUrgent = xmobarColor "#D65D0E" "" . wrap "!" "!"  -- Urgent workspace
                , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                --, ppExtras  = [windowCount]                           -- # of windows current workspace
                }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------


lowerVolume         = "<XF86AudioLowerVolume>"
lowerVolumeCMD      = "amixer set Master 2%- && volnoti-show $(amixer get Master | grep -Po '[0-9]+(?=%)' | head -1)"

raiseVolume         = "<XF86AudioRaiseVolume>"
raiseVolumeCMD      = "amixer set Master 2%+ && volnoti-show $(amixer get Master | grep -Po '[0-9]+(?=%)' | head -1)"

muteVolume          = "<XF86AudioMute>"
muteVolumeCMD       = "amixer set Master toggle && if amixer get Master | grep -Fq '[off]'; then volnoti-show -m; else volnoti-show $(amixer get Master | grep -Po '[0-9]+(?=%)' | head -1); fi"

backLightUp         = "<XF86AudioNext>"
backLightUpCMD      = "light -A 0.2"

backLightDown       = "<XF86AudioPrev>"
backLightDownCMD    = "light -U 0.2"

myKeys = [ (raiseVolume     , spawn raiseVolumeCMD               ) -- raise volume
         , (lowerVolume     , spawn lowerVolumeCMD               ) -- lower volume
         , (muteVolume      , spawn muteVolumeCMD                ) -- mute volume
         , (backLightUp     , spawn backLightUpCMD               ) -- Backlight up
         , (backLightDown   , spawn backLightDownCMD             ) -- Backlight down
         , ("M-S-b"         , spawn "GTK_THEME=Adwaita:dark firefox"                ) -- launch browser
         , ("M-<Return>"    , spawn myTerminal                   ) -- launch terminal
         , ("M-d"           , spawn "rofi -show run"             ) -- launch rofi
         -- , ("M-S-e"           , spawn "emacs"             ) -- launch emacs
         , ("M-S-e"           , spawn "emacsclient -c -a Emacs"             ) -- launch emacs
         ]


--myTerminal  = "alacritty"
-- myTerminal  = "alacritty -e tmux"
myTerminal :: String
myTerminal  = "kitty -e tmux"

myBorderWidth :: Dimension
myBorderWidth = 1

-- myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces :: [String]
myWorkspaces = ["Term", "Emacs", "Web" ] ++ map show [4..9]

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- myManageHook = composeAll
--     [ className =? "MPlayer"        --> doFloat
--     , className =? "Gimp"           --> doFloat
-- --    , className =? "display"        --> doFloat
--     , className =? "Wpa_gui"        --> doFloat
--     , resource  =? "desktop_window" --> doIgnore
--     , resource  =? "kdesktop"       --> doIgnore
--     , title     =? "zsh-onetime"    --> doFloat
--     , manageDocks
--     , scratchpadManageHook (W.RationalRect 0.125 0.25 0.75 0.5)
--     ]

-- myManageHook = composeAll . concat $ [
--     [ className =? "firefox" --> doShift ( myWorkspaces !! 7 ) ]
--   , [ className =? "emacs" --> doShift ( myWorkspaces !! 3 ) ]
--   -- and so on
--   ]

-- myManageHook = composeAll . concat $ [
--     [ className =? "firefox" --> doShift "Web" ]
--   , [ className =? "emacs" --> doShift "Emacs" ]
--   -- and so on
--   ]

-- Example from: https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips
myManageHook = composeAll
   [ className =? "firefox" --> doShift "Web"
   , className =? "Emacs"      --> doShift "Emacs"
   -- , title =? "Oracle VM VirtualBox Manager"     --> doFloat
   , manageDocks
   ]


-- myFont = "xft:Fira Mono for Powerline:style=Bold:size=11:antialias=true, Font Awesome 5 Free Solid:style=solid:size=10"

--myLayout = smartSpacing 2 $ (tiled ||| Full)
-- myLayoutHook = avoidStruts $ smartSpacing 1 $ myLayout

-- myLayoutHook = avoidStruts $ mouseResize $ spacing 1 $ myLayout
--              where

--                myLayout = tiled ||| Full
--                               where
--                               -- tiled = Tall nmaster delta ratio
--                               tiled = Tall nmaster delta ratio
--                --     -- The default number of windows in the master pane
--                               nmaster = 1
--                --     -- Default proportion of screen occupied by master pane
--                               ratio = 1/2
--                --     -- Percent of screen to increment by when resizing panes
--                               delta = 3/100

tall       = renamed [Replace "tall"]
             -- $ limitWindows 12       -- This need "import XMonad.Layout.LimitWindows"
             $ spacing 1
             $ ResizableTall 1 (3/100) (1/2) []

myLayoutHook = avoidStruts $ mouseResize $ spacing 1 $ myLayout
             where

               myLayout = named "Tall" tall ||| Full


-- -- Theme for showWName which prints current workspace when you change workspaces.
-- myShowWNameTheme :: SWNConfig
-- myShowWNameTheme = def
--     { swn_font              = "xft:IBM 3270:bold:size=40"
--     -- , swn_font              = "xft:Ubuntu:bold:size=40"
--     , swn_fade              = 1.0
--     , swn_bgcolor           = "#3c3836"
--     , swn_color             = "#689D6A"
--     }


myConfig = defaultConfig
    { terminal    = myTerminal
    , modMask     = mod4Mask
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColour
    , focusedBorderColor = myNormalBorderColour
    --, layoutHook         = avoidStruts $ myLayout
    --, layoutHook         = showWName' myShowWNameTheme myLayoutHook
    , layoutHook         = myLayoutHook
    , workspaces         = myWorkspaces
    , manageHook         = myManageHook
    -- , startupHook        = myStartupHook
    --, logHook            = xmobarPP
    }
    `additionalKeysP`      myKeys

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
-- main = xmonad =<< myBar myPP toggleStrutsKey myConfig

--main = do
--    xmonad $ myConfig
