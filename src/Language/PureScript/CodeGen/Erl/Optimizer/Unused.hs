-- |
-- Removes unused variables
--
module Language.PureScript.CodeGen.Erl.Optimizer.Unused
  ( removeUnusedArg
  ) where

import Prelude.Compat

import Language.PureScript.CodeGen.Erl.AST
import qualified Language.PureScript.Constants as C

removeUnusedArg :: Erl -> Erl
removeUnusedArg = everywhereOnErl convert
  where
  convert (EFun name arg body) | arg == C.__unused =
    EFunFull name [(EFunBinder [] Nothing, body)]
  convert js = js
