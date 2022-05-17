{-#LANGUAGE BangPatterns #-}
module Main where

import Control.Exception
import Control.Monad
import Dzang.Interpreter
import Dzang.Language
import Dzang.Typing.TypeChecker
import Dzang.Typing.Types
import Options.Applicative

data CmdLineArgs = CLA
  { debug :: Bool,
    eval :: Bool,
    parse :: Bool,
    typeIt :: Bool
  }

data InterpreterResult = IR Value PolyType

instance Show InterpreterResult where
  show (IR v pt) = show v <> ": " <> show pt

main :: IO ()
main = execParser opts >>= main'
  where
    opts =
      info
        (cmdLineParser <**> helper)
        (fullDesc <> progDesc "Dzang interpreter environment")

main' :: CmdLineArgs -> IO ()
main' (CLA False False False False) = forever' evalEval
main' (CLA True _ _ _) = forever' (pretty . debugDzang)
main' (CLA _ True _ _) = forever' evalEval
main' (CLA _ _ True _) = forever' (pretty . parseDzang)
main' (CLA _ _ _ True) =
  forever'
    ( \s ->
        let expr = parseDzang s
            !pt = runTypeChecker expr
            res = runExpr expr
         in pretty $ IR res pt
    )

pretty :: Show a => a -> IO ()
pretty a = putStrLn $ "→ " <> show a

forever' :: (String -> IO ()) -> IO ()
forever' f = forever $ do
  l <- getLine
  res <- try (f l) :: IO (Either SomeException ())
  case res of
    Left err -> print err
    Right _ -> return ()

cmdLineParser :: Parser CmdLineArgs
cmdLineParser =
  CLA
    <$> switch
      ( long "debug"
          <> short 'd'
          <> help
            "Debugmode for the Dzang parser printing the resulting parser state"
      )
    <*> switch
      ( long "eval" <> short 'e'
          <> help
            "Dzang evaluator interpreting user input"
      )
    <*> switch
      ( long "parse" <> short 'p'
          <> help
            "Dzang parser returning the result of a parse operation"
      )
    <*> switch
      ( long "type" <> short 't'
          <> help
            "Dzang parser returning the type of the given expression"
      )
