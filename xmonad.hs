import XMonad
import qualified XMonad.Actions.FlexibleResize as Flex 
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog -- (dynamicLogWithPP, xmobarPP, PP(ppOutput))
import XMonad.Hooks.EwmhDesktops        -- ewmh, fullscreenEventHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook)
import XMonad.Layout.Circle
import XMonad.Layout.DwmStyle
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.LayoutScreens      -- layoutSplitScreen
import XMonad.Layout.Mosaic
import XMonad.Layout.OneBig
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Tabbed
import XMonad.Actions.Search (selectSearchBrowser, google)
import XMonad.Layout.TwoPane
import XMonad.Prompt                    -- XPConfig
import XMonad.Prompt.Shell              -- shellPrompt
import XMonad.Util.EZConfig             -- additionalKeysP
import XMonad.Util.Run (unsafeSpawn,spawnPipe, hPutStrLn)
import XMonad.Util.WorkspaceCompare     -- getSortByXineramaRule
import XMonad.Layout.Magnifier

baseConfig = desktopConfig

main :: IO ()
main = do
  myStatusBar <- spawnPipe "xmobar"
	xmonad $ ewmh $ baseConfig {
    terminal = "urxvt",
    modMask = myModMask,
    manageHook      = myManageHook,
    handleEventHook = myHandleEventHook,
    layoutHook = myLayout,
    logHook = myLogHook myStatusBar
  }
                 `additionalKeysP` myAdditionalKeysP

myLayout = spacing 2 $ avoidStruts $ dwmStyle shrinkText defaultTheme
  (spiral(6/7) ||| TwoPane (1/55) (1/2) ||| OneBig (3/4) (3/4) ||| Full |||
   Circle ||| (magnifiercz 1.5 $ mosaic 2 [3,2]) ||| simpleTabbed
  )
  where
  tiled = Tall 1 (3 / 100) (1 / 2)

-- TwoPane (1/55) (1/2) ||| Mirtror tiled ||| ThreeColMid 1 (3/100) (1/2) ||| multiCol [1] 4 0.01 0.5 ||| 
-- ||| MosaicAlt M.empty ||| Grid False 
myModMask = mod4Mask

myManageHook =   insertPosition Below Newer
             <+> manageDocks

myLayoutHook = avoidStruts $ layoutHook defaultConfig

myLogHook h = dynamicLogWithPP xmobarPP {
                    ppSep    = " | "
                  , ppTitle  = xmobarColor "green" "" . shorten 80
                  , ppOutput = hPutStrLn h
                  , ppSort   = getSortByXineramaRule
                }

myHandleEventHook =   docksEventHook
                  <+> fullscreenEventHook

myAdditionalKeysP = [
    ("M-f",   unsafeSpawn "firefox"),
    ("M-v",   unsafeSpawn "vivaldi"),
    ("M-g",   unsafeSpawn "gnumeric"),
    ("M-r",   unsafeSpawn "geany"),
    ("M-p",   shellPrompt myXPConfig)

   , ("M-s", goToSelected defaultGSConfig)
  , ("M-x", selectSearchBrowser "/usr/bin/firefox" google)
    ]

myXPConfig = defaultXPConfig {
                   font              = "xft:sans-serif:size=13"
                 , bgColor           = "black"
                 , fgColor           = "grey"
                 , promptBorderWidth = 0
                 , position          = Top
                 , alwaysHighlight   = True
                 , height            = 30
                 }
