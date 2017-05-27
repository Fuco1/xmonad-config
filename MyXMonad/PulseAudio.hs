module MyXMonad.PulseAudio
       ( muteSinkInput
       , setVolumeSinkInput
       , paIncreaseVolumeRunning
       , paDecreaseVolumeRunning
       , paSetVolumeRunning
       ) where

import Data.List (find, sortBy)
import Data.Function (on)
import Text.Parsec
import Text.Parsec.String (Parser)
import XMonad hiding (state)
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

import qualified MyXMonad.Constants
import MyXMonad.Utils

data PAPrompt = PAPrompt String
instance XPrompt PAPrompt where
    showXPrompt (PAPrompt s) = s ++ ": "

data SinkState = Running | Corked deriving (Show, Eq, Ord)

data SinkInput = SinkInput { index :: Int
                           , name :: String
                           , vol :: Int
                           , muted :: Bool
                           , state :: SinkState
                           } deriving (Show)

data MuteCmd = Mute | Unmute | Toggle deriving (Show, Eq)

withSinks :: String -> (SinkInput -> X ()) -> X ()
withSinks prompt action = do
  inputs <- liftIO getSinkInputs
  case inputs of
    Left x -> liftIO $ notifySend 5 "Error" $ show x
    Right [] -> liftIO $ notifySend 5 "Error" "No sinks available."
    Right [s] -> action s
    Right sinks -> do
      let sinks' = sortBy (compare `on` state) sinks
      pick <- sinkPicker sinks' prompt
      let sink = case pick of
                   Just "" -> Just $ head sinks'
                   Just x -> find ((==) x . name) sinks'
                   Nothing -> Nothing
      case sink of
        Just s -> action s
        _ -> return ()

sinkCompletionFunc :: [SinkInput] -> String -> IO [String]
sinkCompletionFunc sinks pick = return $ map (\x -> name x ++ " [" ++ show (state x) ++ "]") picked
  where picked = filter (matchAllWords pick . strToLower . name) sinks

sinkPicker :: [SinkInput] -> String -> X (Maybe String)
sinkPicker sinks prompt = mkXPromptWithReturn
                          (PAPrompt prompt)
                          MyXMonad.Constants.prompt
                          (sinkCompletionFunc sinks)
                          return

muteSinkInput :: X ()
muteSinkInput = withSinks "Mute sink" (\s -> liftIO $ pacmdMute s Toggle)

setVolume :: SinkInput -> X ()
setVolume sink = do
  volume <- mkXPromptWithReturn
              (PAPrompt $ "Volume [" ++ name sink ++ ", " ++ show (vol sink) ++ "%]")
              MyXMonad.Constants.prompt
              (mkComplFunFromList $ map show [0..100])
              return
  liftIO $ case volume of
    Just "" -> pacmdMute sink Toggle
    Just v -> pacmdSetVolume sink v
    Nothing -> return ()

setVolumeSinkInput :: X ()
setVolumeSinkInput = withSinks "Select sink" setVolume

-- | index, mute cmd
pacmdMute :: SinkInput -> MuteCmd -> IO ()
pacmdMute sink cmd = do
  let toggle = case cmd of
        Mute   -> "1"
        Unmute -> "0"
        Toggle -> let m = muted sink in if m then "0" else "1"
  safeSpawn "pacmd" ["set-sink-input-mute", show $ index sink, toggle]

pacmdSetVolume :: SinkInput -> String -> IO ()
pacmdSetVolume sink level =
  safeSpawn "pactl" ["set-sink-input-volume", show $ index sink, "--", level ++ "%"]

paChangeVolumeRunning :: String -> IO ()
paChangeVolumeRunning delta = do
  sinks <- getSinkInputs
  case sinks of
   Right s@(_:_) -> do
     let running = filter ((== Running) . state) s
     mapM_ (\x -> safeSpawn "pactl"
                  ["set-sink-input-volume", show $ index x, "--", delta])
       running
   _ -> return ()

paIncreaseVolumeRunning :: X ()
paIncreaseVolumeRunning = paSetVolumeRunning "+5%"

paDecreaseVolumeRunning :: X ()
paDecreaseVolumeRunning = paSetVolumeRunning "-5%"

paSetVolumeRunning :: String -> X ()
paSetVolumeRunning = liftIO . paChangeVolumeRunning

----------------------------------------
-- parsing of the "pacmd" output... blerg
number :: Parser Int
number = do ds <- many1 digit
            return (read ds)
         <?> "number"

welcome :: Parser String
welcome = string "Welcome to PulseAudio! Use \"help\" for usage information.\n>>> "

sinksAvailable :: Parser String
sinksAvailable = string "sink input(s) available.\n"

numberOfSinks :: Parser Int
numberOfSinks = do
  num <- number
  char ' '
  sinksAvailable
  return num

yesno :: Parser Bool
yesno = (string "yes" >> return True) <|>
        (string "no" >> return False)

volume :: Parser Int
volume = do
  many (char ' ')
  number

sinkState :: Parser SinkState
sinkState = (string "CORKED" >> return Corked) <|>
            (string "RUNNING" >> return Running)

sink :: Parser SinkInput
sink = do
  manyTill anyChar (try (string "index: "))
  index <- number
  manyTill anyChar (try (string "state: "))
  state <- sinkState
  manyTill anyChar (try (string "volume: 0:"))
  vol <- volume
  manyTill anyChar (try (string "muted: "))
  muted <- yesno
  manyTill anyChar (try (string "application.name = \""))
  name <- try alsaPluginName <|> manyTill anyChar (try (char '"'))
  return $ SinkInput index name vol muted state

alsaPluginName :: Parser String
alsaPluginName = do
  manyTill (noneOf "\n") (try (char '['))
  name <- manyTill anyChar (try (char ']'))
  manyTill anyChar (try (char '"'))
  return name

sinkInput :: Parser [SinkInput]
sinkInput = do
  welcome
  nos <- numberOfSinks
  if nos == 0
    then return []
    else count nos sink

getSinkInputs :: IO (Either ParseError [SinkInput])
getSinkInputs = do
  output <- runProcessWithInput "pacmd" ["list-sink-inputs"] ""
  return $ parse sinkInput "" output
