module Main where

import           Data.Maybe
import qualified System.Environment as Env
import           Text.Read

import           Decoy (runDecoyServer)

main :: IO ()
main = do
  mPort <- (readMaybe =<<) <$> Env.lookupEnv "DECOY_PORT"
  rulesFiles <- foldMap (filter (not . null) . words)
            <$> Env.lookupEnv "DECOY_RULE_FILES"
  runDecoyServer (fromMaybe 9000 mPort) rulesFiles
