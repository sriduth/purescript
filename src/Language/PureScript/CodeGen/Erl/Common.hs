-- |
-- Common code generation utility functions
--
module Language.PureScript.CodeGen.Erl.Common where

import Data.Char
import Data.List (intercalate)

import Language.PureScript.Crash
import Language.PureScript.Names

import Language.PureScript.CodeGen.Erl.AST

runAtom :: Atom -> String
runAtom at = case at of
  Atom (Just q) a -> atom q ++ ":" ++ atom a
  Atom Nothing a -> atom a

atom :: String -> String
atom s
  | isValidAtom s = s
  | otherwise = "'" ++ concatMap replaceChar s ++ "'"
  where
  replaceChar '\'' = "\\'"
  replaceChar c = [c]

  isValidAtom [] = False
  isValidAtom a@(fc:_) = isLower fc && all atomChar a && not (nameIsErlReserved a)

  atomChar '_' = True
  atomChar '@' = True
  atomChar c = isAlpha c && isAscii c

atomModuleName :: ModuleName -> String
atomModuleName (ModuleName pns) = intercalate "_" ((toAtomName . runProperName) `map` pns)


toAtomName :: String -> String
toAtomName (h:t) = toLower h : t
toAtomName [] = []

toVarName :: String -> String
toVarName (h:t) =
  let chars = toUpper h : t
  in concatMap replaceChar chars
  where
    replaceChar '.' = "@_"
    replaceChar '$' = "@dollar"
    replaceChar '\'' = "@prime"
    replaceChar x = [x]
toVarName [] = []

identToAtomName :: Ident -> String
identToAtomName = toAtomName . runIdent

identToVar :: Ident -> String
identToVar = toVarName . runIdent


-- |
-- Checks whether an identifier name is reserved in Erlang.
--
nameIsErlReserved :: String -> Bool
nameIsErlReserved name =
  name `elem` erlAnyReserved

erlAnyReserved :: [String]
erlAnyReserved = [
  "after",
  "and",
  "andalso",
  "band",
  "begin",
  "bnot",
  "bor",
  "bsl",
  "bsr",
  "bxor",
  "case",
  "catch",
  "cond",
  "div",
  "end",
  "fun",
  "if",
  "let",
  "not",
  "of",
  "or",
  "orelse",
  "receive",
  "rem",
  "try",
  "when",
  "xor"
  ]
