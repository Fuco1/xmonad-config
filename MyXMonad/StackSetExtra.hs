module MyXMonad.StackSetExtra
       ( currentTag
       , onScreen
       , isOnScreen
       , wsTagToScreenId
       , cmpByScreenId
       , withScreen
       , withOtherScreen
       , shiftAndView
       , shiftAndGreedyView
       , shiftAndViewAtOther
       , shiftToOtherScreen
       ) where

import Control.Monad (liftM2)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import XMonad

import qualified XMonad.StackSet as W

-- | Tag of workspace currently selected.
currentTag = W.tag . W.workspace

-- | Return list of workspaces which are visible.
onScreen = liftM2 (:) W.current W.visible

-- | True if workspace is visible.
isOnScreen a w  = a `elem` map currentTag (onScreen w)

-- | Convert workspace tag to screen id.
wsTagToScreenId
  s -- | List of visible screens
  x -- | Workspace tag
  = W.screen $ fromJust $ find ((== x) . currentTag) s

-- | Compare two workspaces by their screen id
cmpByScreenId st = comparing (wsTagToScreenId $ onScreen st)

-- | Modify the current window list with a pure function in context of a named
-- screen, then refresh.  See also 'XMonad.Operations.windows'.
withScreen :: ScreenId -- ^ ID of the target screen.  If such doesn't exist, this operation is NOOP
              -> (WorkspaceId -> WindowSet -> WindowSet)
              -> X ()
withScreen n f = screenWorkspace n >>= flip whenJust (windows . f)

-- | Transform pure function to one running in context of the other screen.  The
-- function takes 'WorkspaceId' of the workspace current on the other screen.
-- This only works in case of two physical displays.
withOtherScreen :: (WorkspaceId -> WindowSet -> WindowSet) -> WindowSet -> WindowSet
withOtherScreen f st = case W.visible st of
  []      -> st
  [other] -> let ws = currentTag $ other in f ws st
  _       -> st

-- | Shift the current active window to specified workspace, then select it on
-- the other screen.
shiftAndView :: WorkspaceId -> WindowSet -> WindowSet
shiftAndView = liftM2 (.) W.view W.shift

-- | Shift the current active window to specified workspace, then select it on
-- the current screen.
shiftAndGreedyView :: WorkspaceId -> WindowSet -> WindowSet
shiftAndGreedyView = liftM2 (.) W.greedyView W.shift

-- For those wondering, liftM2 (.) f g = \x -> f x . g x

-- | Shift the current active window to specified workspace, then select it on
-- the other screen.
shiftAndViewAtOther :: WorkspaceId -> WindowSet -> WindowSet
shiftAndViewAtOther wid = withOtherScreen (\ows -> W.view wid . W.view ows . W.shift wid)

-- | Shift the current active window to the workspace active on the other screen
-- and select it immediately.
shiftToOtherScreen :: WindowSet -> WindowSet
shiftToOtherScreen = withOtherScreen shiftAndView
