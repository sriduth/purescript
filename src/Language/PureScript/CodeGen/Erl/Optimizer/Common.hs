module Language.PureScript.CodeGen.Erl.Optimizer.Common where

import Prelude.Compat
import Data.Text (Text)

import Language.PureScript.CodeGen.Erl.AST

isFn :: (Text, Text) -> Erl -> Bool
isFn (moduleName, fnName) (EApp (EAtomLiteral (Atom (Just x) y)) []) =
  x == moduleName && y == fnName
isFn _ _ = False

isDict :: (Text, Text) -> Erl -> Bool
isDict (moduleName, dictName) (EApp (EAtomLiteral (Atom (Just x) y)) []) = x == moduleName && y == dictName
isDict _ _ = False

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)
