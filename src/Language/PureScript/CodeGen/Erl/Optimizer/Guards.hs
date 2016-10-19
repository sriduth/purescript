-- |
-- This module provides collapsing of simple guard expressions
--
module Language.PureScript.CodeGen.Erl.Optimizer.Guards where

import Prelude.Compat

import Language.PureScript.CodeGen.Erl.AST

inlineSimpleGuards :: Erl -> Erl
inlineSimpleGuards = id
