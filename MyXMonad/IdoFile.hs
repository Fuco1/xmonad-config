module MyXMonad.IdoFile
       (
         idoFilePrompt
       , myIdoFilePrompt
       ) where

import XMonad hiding (state)
import XMonad.Prompt
import System.Directory
import Control.Applicative
import Data.Char (toLower)
import Data.Map (insert)
import Data.IORef

import qualified MyXMonad.Constants

data IdoFilePrompt = IdoFilePrompt String
instance XPrompt IdoFilePrompt where
    showXPrompt (IdoFilePrompt s) = s ++ ": "

-- | Get file or directory with ido-like prompt
idoFilePrompt :: FilePath -- ^ Initial directory
                 -> XPConfig -- ^ Prompt config
                 -> X (Maybe FilePath)
idoFilePrompt initial xpConfig = do
  files <- liftIO $ (:) "." . filter (`notElem` [".", ".."] ) <$> getDirectoryContents initial
  ref <- liftIO $ newIORef Nothing
  file <- mkXPromptWithReturn
   (IdoFilePrompt "Find file")
   (xpConfig { promptKeymap = insert (0, xK_BackSpace) (idoFileDeleteString ref) (promptKeymap xpConfig) })
   (idoFileCompletion files)
   return
  goUp <- liftIO $ readIORef ref
  if goUp == Just ".."
    then idoFilePrompt (initial ++ "/../") xpConfig
    else
      case file of
       Nothing -> return Nothing
       Just "" -> normalize initial
       Just "." -> normalize initial
       Just f -> do
         let full = initial ++ "/" ++ f
         isFile <- liftIO $ doesFileExist full
         if isFile
           then
             normalize full
           else
             idoFilePrompt full xpConfig
  where
    normalize = fmap Just . liftIO . canonicalizePath
    done = setSuccess True >> setDone True
    idoFileDeleteString ref = do
      input <- getInput
      if input == ""
        then do
          liftIO $ writeIORef ref (Just "..")
          done
        else deleteString Prev

idoFileCompletion :: [FilePath] -> String -> IO [FilePath]
idoFileCompletion paths pattern = return $ filter
                                   (isSubsequenceOf (strToLower pattern) . strToLower)
                                   paths

myIdoFilePrompt :: FilePath -> X (Maybe FilePath)
myIdoFilePrompt = flip idoFilePrompt MyXMonad.Constants.prompt

----------- Utility
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

strToLower :: String -> String
strToLower = map toLower
