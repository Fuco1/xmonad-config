module MyXMonad.Interactive where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Prompt
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import MyXMonad.Utils
import MyXMonad.Constants as C

newtype InteractiveSpecification a = InteractiveSpecification (MaybeT X a)

instance Functor InteractiveSpecification where
  fmap f (InteractiveSpecification x) = InteractiveSpecification (fmap f x)

instance Applicative InteractiveSpecification where
  pure x = InteractiveSpecification (pure x)
  (InteractiveSpecification f) <*> (InteractiveSpecification x) = InteractiveSpecification (f <*> x)

spec :: X (Maybe a) -> InteractiveSpecification a
spec = InteractiveSpecification . MaybeT

interactive :: InteractiveSpecification (X ()) -> X ()
interactive (InteractiveSpecification x) = do
  action <- runMaybeT x
  fromMaybe (return ()) action

data InteractivePrompt = InteractivePrompt String
instance XPrompt InteractivePrompt where
    showXPrompt (InteractivePrompt s) = s

-- predefined prompts
prompts :: XPConfig -> String -> InteractiveSpecification String
prompts c s = spec $ mkXPromptWithReturn (InteractivePrompt s) (c { alwaysHighlight = False }) emptyCompl return
 where
   emptyCompl _ = return []

promptS :: XPConfig -> [String] -> String -> InteractiveSpecification String
promptS c options s = spec $ mkXPromptWithReturn (InteractivePrompt s) c (mkComplFunFromList' options) return

promptw :: XPConfig -> String -> InteractiveSpecification Window
promptw c s = spec $ do
  wm <- windowMap
  winName <- mkXPromptWithReturn (InteractivePrompt s) c (compList wm) return
  case winName of
    Nothing -> return Nothing
    Just wn -> return . Just $ (M.!) wm wn
  where
    compList m s = return . filter (searchPredicate c s) . map fst . M.toList $ m

promptW :: XPConfig -> String -> InteractiveSpecification WorkspaceId
promptW c s = spec $ do
  ws <- gets windowset
  mkXPromptWithReturn (InteractivePrompt s) c (compList $ W.workspaces ws) return
  where
    compList m s = return . filter (searchPredicate c s) . map W.tag $ m

--------- USAGE
-- copied here from Utils
-- notifySend :: Int -> String -> String -> IO ()
-- notifySend timeout title msg =
--   void $ readProcess "notify-send" ["-u", "low", "-t", show (timeout * 1000), title, msg] ""

cf = C.prompt -- XPConfig

notify :: String -> String -> X ()
notify title body = liftIO $ notifySend 5 title body

dm :: String -> X ()
dm msg = spawn $ "dmenu -p 'Test: ' '" ++ msg ++ "'"

inotify = interactive $ spawn <$> prompts cf "Shell: "

inotify2 = interactive $ notify <$> prompts cf "Title: " <*> promptS cf ["One", "Other", "Lorem ipsum"] "Body: "

mWindow :: Window -> WorkspaceId -> X ()
mWindow w wi = windows (W.shiftWin wi w)

iMWindow = interactive $ mWindow <$> promptw cf "Window to move: " <*> promptW cf "To workspace: "

urxvtc = interactive $ (\x y -> spawn $ y ++ " -name " ++ x) <$> prompts cf "String: " <*> pure "urxvtc"
