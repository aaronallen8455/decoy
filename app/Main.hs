module Main where

import           Data.Maybe
import qualified System.Environment as Env
import           Text.Read

import           Decoy (runDecoyServer)

main :: IO ()
main = do
  mPort <- (readMaybe =<<) <$> Env.lookupEnv "PORT"
  mRulesFile <- Env.lookupEnv "RULES_FILE"
  runDecoyServer (fromMaybe 9000 mPort)
                 mRulesFile
