module MyXMonad.Utils
       ( subseq
       , matchAllLists
       , matchAllWords
       , getFileName
       , strToLower
       , recompileXMonad
       , (<%>)
       , notifySend
       , runInTermOrRaise
       , whenJust_
       ) where

import Control.Monad (void)
import Data.Char (toLower)
import Data.List (and, isInfixOf)
import System.Process (readProcess)
import XMonad
import XMonad.Actions.WindowGo (raiseMaybe)
import XMonad.Util.Run (spawnPipe, runInTerm)
import qualified XMonad.StackSet as W

subseq :: Eq a => [a] -> [a] -> Bool
[]     `subseq` _      = True
(_:_ ) `subseq` []     = False
(a:as) `subseq` (b:bs) = (if a == b then as else a:as) `subseq` bs

-- | Return true if every list in second argument is a sublist of first.
matchAllLists :: Eq a => [[a]] -> [a] -> Bool
matchAllLists ws w = and (map (flip isInfixOf w) ws)

matchAllWords :: String -> String -> Bool
matchAllWords = matchAllLists . words

getFileName :: String -> String
getFileName = reverse . takeWhile (/= '/') . reverse

strToLower :: String -> String
strToLower = map toLower

recompileXMonad :: X ()
recompileXMonad = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

infixr 6 <%>
(<%>) :: String -> String -> String
(<%>) = ((++) . (flip (++) " "))

notifySend :: Int -> String -> String -> IO ()
notifySend timeout title msg =
  void $ readProcess "notify-send" ["-u", "critical", "-t", show (timeout * 1000), title, msg] ""

runInTermOrRaise :: String -> WorkspaceId -> X ()
runInTermOrRaise prog ws = raiseMaybe (runInTerm ("-name " ++ prog) prog >> windows (W.greedyView ws)) (resource =? prog)

-- TODO: add submap support

whenJust_ :: Monad m => m () -> Maybe a -> m ()
whenJust_ = flip whenJust . const
