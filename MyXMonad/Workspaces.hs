{-# LANGUAGE DeriveDataTypeable #-}
module MyXMonad.Workspaces
       ( withWorkspaces
       , getMyCompare
       , getSortByMyCompare
       , ScreenOrder(..)
       ) where

import XMonad
import XMonad.Util.WorkspaceCompare (getWsCompare, mkWsSort, WorkspaceCompare, WorkspaceSort)

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import qualified MyXMonad.StackSetExtra   as WX

data ScreenOrder = ScreenOrder [ScreenId] deriving Typeable
instance ExtensionClass ScreenOrder where
  initialValue = ScreenOrder [2,0,1]

-- | Map a function under keys to workspaces
withWorkspaces :: [WorkspaceId] -- ^ List of workspaces.
                  -> [KeySym] -- ^ List of keys.  Key at nth position
                              -- corresponds to nth workspace.
                  -> ButtonMask -- ^ Modifier mask.  Pass 0 for no mask.
                  -> (WorkspaceId -> WindowSet -> WindowSet)
                  -> [((ButtonMask, KeySym), X ())]
withWorkspaces wids keys mask f = [ ((mask, key), windows $ f ws) | (ws, key) <- zip wids keys]

-- | Compare workspaces by visibility, screenid and workspace index.
getMyCompare :: X WorkspaceCompare
getMyCompare = do
    w <- gets windowset
    wsCompare <- getWsCompare
    ScreenOrder screens <- (XS.get :: X ScreenOrder)
    return $ \a b -> case (WX.isOnScreen a w, WX.isOnScreen b w) of
        (True, True)   -> compareScreen screens (WX.onScreen w) a b
        (False, False) -> wsCompare a b
        (True, False)  -> LT
        (False, True)  -> GT
  where
    compareScreen
      screens -- | Real physical screen order, this is determined on loading
      vsi -- | Visible screens IDs
      = comparing
      (\x -> fromJust . elemIndex (WX.wsTagToScreenId vsi x) $ screens)

-- | Sort workspaces by visibility, screenid and workspace index.
getSortByMyCompare :: X WorkspaceSort
getSortByMyCompare = mkWsSort getMyCompare
