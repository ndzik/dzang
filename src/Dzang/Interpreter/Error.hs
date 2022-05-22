module Dzang.Interpreter.Error where

import Dzang.Typing.Error

data InterpreterError = TE TypeError | EE String | PE String

instance Show InterpreterError where
  show (TE te) = "TYPEERROR: " <> show te
  show (EE msg) = "EVALERROR: " <> msg
  show (PE msg) = "PARSEERROR: " <> msg
