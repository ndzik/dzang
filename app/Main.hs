module Main where

import           Control.Monad                  ( void )
import           Dzang
import           Options.Applicative

data CmdLineArgs = CLA
  { interpret :: Bool
  , debug     :: Bool
  }

main :: IO ()
main = execParser opts >>= main'
 where
  opts = info (cmdLineParser <**> helper)
              (fullDesc <> progDesc "Dzang interpreter environment")

main' :: CmdLineArgs -> IO ()
main' (CLA False False) = void $ runInterpreter forever emptyInterpreterState
main' (CLA True  _    ) = void $ runInterpreter forever emptyInterpreterState
main' (CLA _ True) =
  void $ runInterpreter (forever' debugLog) emptyInterpreterState

cmdLineParser :: Parser CmdLineArgs
cmdLineParser =
  CLA
    <$> switch
          (  long "interpret"
          <> short 'i'
          <> help
               "Dzang interpreter running in default mode, parsing input and returning their result and type."
          )
    <*> switch
          (  long "debug"
          <> short 'd'
          <> help
               "Debugmode for the Dzang parser printing the resulting parser state."
          )
