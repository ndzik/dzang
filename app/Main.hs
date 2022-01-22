module Main where

import           Control.Exception
import           Control.Monad
import           Dzang.Interpreter
import           Dzang.Language
import           Dzang.TypeChecker
import           Options.Applicative

data CmdLineArgs = CLA
  { debug :: Bool
  , eval  :: Bool
  , parse :: Bool
  , check  :: Bool
  }

main :: IO ()
main = execParser opts >>= main'
 where
  opts = info (cmdLineParser <**> helper)
              (fullDesc <> progDesc "Dzang interpreter environment")

main' :: CmdLineArgs -> IO ()
main' (CLA False False False False) = forever' evalEval
main' (CLA True  _     _     _    ) = forever' (print . debugDzang)
main' (CLA _     True  _     _    ) = forever' evalEval
main' (CLA _     _     True  _    ) = forever' (print . parseDzang)
main' (CLA _     _     _     True ) = forever' (print . runChecker)

forever' :: (String -> IO ()) -> IO ()
forever' f = forever $ do
  l   <- getLine
  res <- try (f l) :: IO (Either SomeException ())
  case res of
    Left  err -> print err
    Right _   -> return ()

cmdLineParser :: Parser CmdLineArgs
cmdLineParser =
  CLA
    <$> switch
          (  long "debug"
          <> short 'd'
          <> help
               "Debugmode for the Dzang parser printing the resulting parser state"
          )
    <*> switch
          (long "eval" <> short 'e' <> help
            "Dzang evaluator interpreting user input"
          )
    <*> switch
          (long "parse" <> short 'p' <> help
            "Dzang parser returning the result of a parse operation"
          )
    <*> switch
          (long "check" <> short 'c' <> help
            "Dzang typechecker"
          )
