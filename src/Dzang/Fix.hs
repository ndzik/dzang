module Dzang.Fix where

newtype Fix f = Fix { unFix :: f (Fix f) }
