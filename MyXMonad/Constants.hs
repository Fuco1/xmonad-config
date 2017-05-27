{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module MyXMonad.Constants
       ( module MyXMonad.Constants
       , MyXMonad.Workspaces.ScreenOrder -- tento reexport je prasarna
       ) where

import Data.List (init, last)

import XMonad
import XMonad.Actions.Prefix (ppFormatPrefix)
import XMonad.Actions.CopyWindow (copyToAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Drawer
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizeScreen
import XMonad.Prompt
import XMonad.Util.Loggers
import XMonad.Util.ExtensibleState as XS

import qualified XMonad.StackSet as W

import MyXMonad.Utils
import MyXMonad.Workspaces

prompt :: XPConfig
prompt = def { font = "xft:Monospace-12:narrow"
             , bgColor = "black"
             , fgColor = "#888a85"
             , fgHLight = "#eeeeec"
             , bgHLight = "#3465a4"
             , borderColor = "#008800"
             , promptKeymap = emacsLikeXPKeymap
             , height = 25
             , searchPredicate = subseq
             , alwaysHighlight = True
             }

workspaces :: [WorkspaceId]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
              "q", "w", "e", "r"]

workspaceKeys = [xK_1 .. xK_9] ++ [xK_0, xK_q, xK_w, xK_e, xK_r]

-- | Just like 'withWorkspaces' but supply default workspace and key lists.
withWorkspacesD :: ButtonMask -> (WorkspaceId -> WindowSet -> WindowSet) -> [((ButtonMask, KeySym), X ())]
withWorkspacesD = withWorkspaces MyXMonad.Constants.workspaces workspaceKeys

layout = mixerdrawer `onBottom` as (Full ||| tiled ||| Mirror tiled)
  where
    mixerdrawer = drawer 0 0.5
                  (foldl1 Or . map Resource $ ["alsamixer", "pacmixer"])
                  (Tall 0 0 0)
    tiled = Tall 1 (3/100) (1/2)
    as = avoidStruts . smartBorders

printer = def { ppCurrent         = xmobarColor "#fcaf3e" ""
              , ppVisible         = xmobarColor "#d3d7cf" ""
              , ppHidden          = id
              , ppHiddenNoWindows = const ""
              , ppUrgent          = xmobarColor "#cc0000" ""
              , ppSep             = "│"
              , ppWsSep           = ""
              , ppTitle           = xmobarColor "#8cc4ff" ""
              , ppLayout          = (:[]) . head
              , ppOrder           = \x -> case x of
                                      [_, _, _] -> x
                                      (a:b:c:rest) -> [last rest, a, b, c]
              , ppExtras          = [ppFormatPrefix]
              , ppSort            = getSortByMyCompare
              }

doKillWindow :: ManageHook
doKillWindow = ask >>= \w -> liftX (killWindow w) >> doF (W.delete w)

manageHook = (composeOne . concat $ --  <&&> resource =? "TeamViewer.exe"
    [ [ (stringProperty "WM_NAME" =? "Computers & Contacts" <&&> resource =? "TeamViewer.exe") -?> doKillWindow
      , isDialog -?> doFloat
      , transience
      , isFullscreen -?> doFullFloat
      , resource =? "TeamViewer.exe" -?> doCenterFloat
      , resource =? "arandr" -?> doCenterFloat
      ]
    , [ className =? c -?> doFloat | c <- myCFloats ]
    , [ title     =? t -?> doFloat | t <- myTFloats ]
    , [ resource  =? r -?> doFloat | r <- myRFloats ]
    , [ className =? c -?> doIgnore | c <- myCIgnores ]
    , [ title     =? t -?> doIgnore | t <- myTIgnores ]
    , [ resource  =? r -?> doIgnore | r <- myRIgnores ]
    , [ className =? r -?> doShift "0" | r <- [
          "spotify", "alsamixer", "pacmixer", "ncmpcpp"
          ]]
    ])
    <+> manageDocks
    <+> XMonad.manageHook def
    where
        myCFloats = ["sun-awt-X11-XFramePeer", "net-sourceforge-jnlp-runtime-Boot", "Unity"]
        myTFloats = ["GLFW-b-demo"]
        myRFloats = []
        myCIgnores = ["Xfce4-notifyd"]
        myTIgnores = []
        myRIgnores = []
