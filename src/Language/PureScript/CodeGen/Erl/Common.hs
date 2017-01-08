-- |
-- Common code generation utility functions
--
module Language.PureScript.CodeGen.Erl.Common
( runAtom
, atomPS
, atom
, atomModuleName
, toAtomName
, toVarName
, identToAtomName
, identToVar
, nameIsErlReserved
) where

import Prelude.Compat hiding (concatMap, all)

import Data.Char
import Data.Text (Text, intercalate, uncons, cons, concatMap, singleton, all, pack, singleton)
import Numeric
import Data.Word (Word16)
import Data.Monoid ((<>))
import Language.PureScript.Names
import Language.PureScript.PSString

import Language.PureScript.CodeGen.Erl.AST

runAtom :: Atom -> Text
runAtom at = case at of
  Atom (Just q) a -> atom q <> ":" <> atom a
  Atom Nothing a -> atom a
  AtomPS (Just q) a -> atom q <> ":" <> atomPS a
  AtomPS Nothing a -> atomPS a

-- Atoms do not support codepoints > 255
atomPS :: PSString -> Text
atomPS a = atom $ foldMap encodeChar (toUTF16CodeUnits a)
  where
    encodeChar :: Word16 -> Text
    encodeChar c | c > 0xFF = "@x" <> hex 4 c -- Can't use real unicode escape
    encodeChar c | c > 0x7E || c < 0x20 = "\\x" <> hex 2 c
    encodeChar c | toChar c == '\b' = "\\b"
    encodeChar c | toChar c == '\t' = "\\t"
    encodeChar c | toChar c == '\n' = "\\n"
    encodeChar c | toChar c == '\v' = "\\v"
    encodeChar c | toChar c == '\f' = "\\f"
    encodeChar c | toChar c == '\r' = "\\r"
    encodeChar c = singleton $ toChar c

toChar :: Word16 -> Char
toChar = toEnum . fromIntegral

hex :: (Enum a) => Int -> a -> Text
hex width c =
  let hs = showHex (fromEnum c) "" in
  pack (replicate (width - length hs) '0' <> hs)

-- Atoms:
-- Must consist entirely of valid Latin-1 characters
-- Unquoted: must start with lower case char and be alpha-numeric, @ or _
-- Quoted: enclosed in single quotes, single quotes must be escaped \'
atom :: Text -> Text
atom s
  | isValidAtom s = s
  | otherwise = "'" <> concatMap replaceChar s <> "'"
  where
  replaceChar '\'' = "\\'"
  replaceChar c | not (isLatin1 c) = "@x" <> hex 4 c
  replaceChar c = singleton c

  isValidAtom a = case uncons a of
    Nothing -> False
    Just (fc, _) -> isLower fc && all atomChar a && not (nameIsErlReserved a)

  atomChar '_' = True
  atomChar '@' = True
  atomChar c = isDigit c || (isLatin1 c && isAlpha c)

atomModuleName :: ModuleName -> Text
atomModuleName (ModuleName pns) = intercalate "_" ((toAtomName . runProperName) `map` pns)

toAtomName :: Text -> Text
toAtomName text = case uncons text of
  Just (h, t) -> cons (toLower h) t
  Nothing -> text

toVarName :: Text -> Text
toVarName v = case uncons v of
  Just (h, t) ->
    replaceFirst h <> concatMap replaceChar t
  Nothing -> v
  where
    replaceChar '.' = "@_"
    replaceChar '$' = "@dollar"
    replaceChar '\'' = "@prime"
    replaceChar c | not (isLatin1 c) = "@x" <> hex 4 c
    replaceChar x = singleton x

    replaceFirst x
      | isAlpha x && isLatin1 x = singleton (toUpper x)
      | x == '_' = "_"
      | otherwise = "V@1" <> replaceChar x

identToAtomName :: Ident -> Text
identToAtomName = toAtomName . runIdent

identToVar :: Ident -> Text
identToVar = toVarName . runIdent


-- |
-- Checks whether an identifier name is reserved in Erlang.
--
nameIsErlReserved :: Text -> Bool
nameIsErlReserved name =
  name `elem` erlAnyReserved

erlAnyReserved :: [Text]
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
