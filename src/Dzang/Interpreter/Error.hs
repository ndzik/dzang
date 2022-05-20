module Dzang.Interpreter.Error where

import Dzang.Typing.Error

data InterpreterError = TE TypeError | EE String

instance Show InterpreterError where
  show (TE te) = "TYPEERROR: " <> show te
  show (EE msg) = "EVALERROR: " <> msg
