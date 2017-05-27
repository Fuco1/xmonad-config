module MyXMonad.Mount where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.List (find, isInfixOf)
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Process (readProcess)
import XMonad
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput, runProcessWithInputAndWait)

import MyXMonad.Constants as Constants (prompt)
import MyXMonad.Utils (matchAllWords, notifySend, strToLower)

data Device = Device { name :: String
                     , uuid :: String
                     , lab :: Maybe String
                     } deriving (Show)

data FieldID = UUID | LABEL | TYPE deriving (Show, Read, Eq)

data Field = Field { fieldId :: FieldID
                   , value :: String
                   } deriving (Show)

field :: Parser Field
field = do
  name <- read <$> manyTill anyChar (try (char '='))
  string "\""
  value <- manyTill anyChar (try (string "\""))
  char ' '
  return $ Field name value

device :: Parser Device
device = do
  dev <- manyTill anyChar (try (char ':'))
  string " "
  fields <- manyTill field (try eof)
  return $ Device
    dev
    (value . fromJust $ find (\(Field id _) -> id == UUID) fields)
    (find (\(Field id _) -> id == LABEL) fields >>= Just . value)


readDevices :: IO (Either ParseError [Device])
readDevices = do
  output <- runProcessWithInput "blkid" [] ""
  -- return output
  return $ mapM (parse device "") $ lines output

data DevicePrompt = DevicePrompt String
instance XPrompt DevicePrompt where
    showXPrompt (DevicePrompt s) = s ++ ": "

mountDevice :: X ()
mountDevice = do
  inputs <- liftIO readDevices
  case inputs of
   Left x -> liftIO $ notifySend 5 "Error" $ show x
   Right devices' -> do
     let devices = filter (isJust . lab) devices'
     device <- mkXPromptWithReturn
               (DevicePrompt "Device: ")
               Constants.prompt
               (deviceCompletionFunc devices)
               return
     case device of
       Just d -> do
         let dev = fromJust $ find (\(Device {lab = Just l'}) -> l' == d) devices
             l = fromJust $ lab dev
             u = uuid dev
             n = name dev
         void $ runProcessWithInput "sudo" ["mount", n, "/media/" ++ l] ""
         liftIO $ notifySend 5 "Notice" $ "Mounted device " ++ n ++ " [" ++ l ++ "]"
       Nothing -> return ()

umountDevice :: X ()
umountDevice = do
  inputs <- liftIO readDevices
  case inputs of
   Left x -> liftIO $ notifySend 5 "Error" $ show x
   Right devices' -> do
     let devices = filter (isJust . lab) devices'
     device <- mkXPromptWithReturn
               (DevicePrompt "Device: ")
               Constants.prompt
               (deviceCompletionFunc devices)
               return
     case device of
       Just d -> do
         let dev = fromJust $ find (\(Device {lab = Just l'}) -> l' == d) devices
             l = fromJust $ lab dev
             u = uuid dev
             n = name dev
         runProcessWithInput "sudo" ["umount", n] ""
         liftIO $ notifySend 5 "Notice" $ "Unmounted device " ++ n ++ " [was " ++ l ++ "]\n"
       Nothing -> return ()

deviceCompletionFunc :: [Device] -> String -> IO [String]
deviceCompletionFunc devices pick = return $ map (fromJust . lab) picked
  where
    picked = filter (isInfixOf pick . strToLower . fromJust . lab) devices
