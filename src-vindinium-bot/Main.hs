{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Options.Applicative

import Vindinium
import FuzzyBot as FB
import BooleanBot as BB

import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (pack, unpack)

data Cmd = Training Settings (Maybe Int) (Maybe Board)
         | Arena Settings
         deriving (Show, Eq)

cmdSettings :: Cmd -> Settings
cmdSettings (Training s _ _) = s
cmdSettings (Arena s) = s

settings :: Parser Settings
settings = Settings <$> (Key <$> argument (str >>= (return . pack)) (metavar "KEY"))
                    <*> (fromString <$> strOption (long "url" <> value "http://vindinium.org"))
                    <*> (fromString <$> strOption (long "bot" <> value "booleanbot"))

trainingCmd :: Parser Cmd
trainingCmd = Training <$> settings
                       <*> optional (option auto (long "turns"))
                       <*> pure Nothing

arenaCmd :: Parser Cmd
arenaCmd = Arena <$> settings

cmd :: Parser Cmd
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        (progDesc "Run bot in arena mode" ))
    )

runCmd :: Cmd -> IO ()
runCmd c  = do
    let settings = cmdSettings c
        botType = settingsBot settings
    s <- runVindinium (cmdSettings c) $ do
        case c of
            (Training _ t b) -> case botType of
                                     "fuzzybot" -> playTraining t b FB.bot FB.initialBotState
                                     _ -> playTraining t b BB.bot BB.initialBotState
            (Arena _)        -> case botType of
                                     "fuzzybot" -> playArena FB.bot FB.initialBotState
                                     _ -> playArena BB.bot BB.initialBotState

    putStrLn $ "Game finished: " ++ unpack (stateViewUrl s)

main :: IO ()
main =
    execParser opts >>= runCmd
  where
    opts = info (cmd <**> helper) idm
