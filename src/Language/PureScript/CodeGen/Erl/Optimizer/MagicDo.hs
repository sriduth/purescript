-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
module Language.PureScript.CodeGen.Erl.Optimizer.MagicDo (magicDo) where

import Prelude.Compat

import Language.PureScript.CodeGen.Erl.AST
import Language.PureScript.CodeGen.Erl.Optimizer.Common
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.CodeGen.Erl.Constants as EC

magicDo :: Options -> Erl -> Erl
magicDo opts | optionsNoMagicDo opts = id
             | otherwise = magicDo'

magicDo' :: Erl -> Erl
magicDo' = everywhereOnErl undo . everywhereOnErlTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad

  convert :: Erl -> Erl
  -- Desugar pure
  convert (EApp (EApp pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  convert (EApp (EApp bind [m]) [EFunFull Nothing [(EFunBinder [] Nothing, e)]]) | isBind bind =
    EFunFull (Just fnName) [(EFunBinder [] Nothing, EBlock (EApp m [] : [ EApp e [] ]))]
  -- Desugar >>=
  convert (EApp (EApp bind [m]) [EFun Nothing "__unused" e]) | isBind bind =
    EFunFull (Just fnName) [(EFunBinder [] Nothing, EBlock (EApp m [] : [ EApp e [] ]))]
  -- TODO check arg is free properly rather than special casing __unused only
  -- convert (EApp (EApp bind [m]) [EFun Nothing var e]) | isBind bind =
  --   EFunFull (Just fnName) [(EFunBinder [] Nothing, EBlock (EVarBind var (EApp m []) : [ EApp e [] ]))]

  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (EApp fn [dict]) | isDict (EC.eff, C.bindEffDictionary) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (EApp fn [dict]) | isDict (EC.eff, C.applicativeEffDictionary) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isFn (EC.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure or return function
  isPurePoly = isFn (EC.controlApplicative, C.pure')

  -- Remove __do function applications which remain after desugaring
  undo :: Erl -> Erl
  undo (EApp (EFunFull (Just ident) [(EFunBinder [] Nothing, body)]) []) | ident == fnName = body
  undo other = other
